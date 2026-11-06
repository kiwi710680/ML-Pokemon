// runner.js — 把頁面裡的 R 程式格（.cell）接到瀏覽器裡的 R（webR, WebAssembly）
// 每一頁共用同一個 R 全域環境；執行某一格前，會先自動執行前面還沒跑過的格（第 0 格是工具箱）。
// 程式都寫在頁面裡；data-files 只用來掛載資料檔。
import { WebR } from 'https://webr.r-wasm.org/v0.5.8/webr.mjs';

const statusEl = document.getElementById('status');
const setStatus = (text, kind = '') => {
  statusEl.textContent = text;
  statusEl.dataset.kind = kind;
};

const cells = [...document.querySelectorAll('.cell')];
const ran = new Array(cells.length).fill(false);
let busy = false;

// 程式格編輯器：CodeMirror 6（R 語法上色＋行號）。CDN 載不到時退回純 textarea，頁面照樣能跑。
// 套件網址與版本寫在每頁 <head> 的 importmap（由 src/build.py 產生），確保每個套件只載一份。
let cm = null;
try {
  const [state, view, commands, language, legacy, highlight] = await Promise.all([
    import('@codemirror/state'),
    import('@codemirror/view'),
    import('@codemirror/commands'),
    import('@codemirror/language'),
    import('@codemirror/legacy-modes/mode/r'),
    import('@lezer/highlight'),
  ]);
  cm = { ...state, ...view, ...commands, ...language, r: legacy.r, tags: highlight.tags };
} catch (e) {
  console.warn('CodeMirror 載入失敗，改用純文字程式格：', e);
}

const rHighlight = cm && cm.HighlightStyle.define([
  { tag: cm.tags.comment, color: '#8a8172', fontStyle: 'italic' },
  { tag: cm.tags.string, color: '#a3341f' },
  { tag: [cm.tags.number, cm.tags.atom, cm.tags.bool], color: '#1b7f4e' },
  { tag: cm.tags.keyword, color: '#1f4fb4', fontWeight: '600' },
  { tag: [cm.tags.standard(cm.tags.variableName), cm.tags.special(cm.tags.variableName)], color: '#0b7285' },
  { tag: cm.tags.operator, color: '#9c4a00' },
  { tag: [cm.tags.paren, cm.tags.bracket, cm.tags.brace], color: '#6b665e' },
]);

// 回傳 { el, get, set, refresh }；onChange 在使用者改程式時呼叫，onRun 給 Ctrl+Enter
function makeEditor(code, label, onChange, onRun) {
  if (cm) try {
    const v = new cm.EditorView({
      doc: code,
      extensions: [
        cm.Prec.highest(cm.keymap.of([{ key: 'Mod-Enter', run: () => { onRun(); return true; } }])),
        cm.lineNumbers(),
        cm.history(),
        cm.drawSelection(),
        cm.bracketMatching(),
        cm.keymap.of([...cm.defaultKeymap, ...cm.historyKeymap]),
        cm.StreamLanguage.define(cm.r),
        cm.syntaxHighlighting(rHighlight),
        cm.EditorState.tabSize.of(2),
        cm.EditorView.contentAttributes.of({ 'aria-label': label, spellcheck: 'false' }),
        cm.EditorView.updateListener.of((u) => { if (u.docChanged) onChange(); }),
      ],
    });
    return {
      el: v.dom,
      get: () => v.state.doc.toString(),
      set: (s) => v.dispatch({ changes: { from: 0, to: v.state.doc.length, insert: s } }),
      refresh: () => v.requestMeasure(),
    };
  } catch (e) {
    console.warn('CodeMirror 建立失敗，改用純文字程式格：', e);
    cm = null;                                   // later cells go straight to the textarea
  }
  const ta = document.createElement('textarea');
  const autosize = () => {
    ta.style.height = 'auto';
    ta.style.height = `${ta.scrollHeight + (ta.offsetHeight - ta.clientHeight) + 2}px`;   // include border + horizontal scrollbar
  };
  ta.spellcheck = false;
  ta.wrap = 'off';                               // long R lines scroll sideways instead of wrapping
  ta.value = code;
  ta.setAttribute('aria-label', label);
  ta.addEventListener('input', () => { autosize(); onChange(); });
  ta.addEventListener('keydown', (e) => {
    if ((e.ctrlKey || e.metaKey) && e.key === 'Enter') {
      e.preventDefault();
      onRun();
    }
  });
  requestAnimationFrame(autosize);               // after the textarea is in the page
  return { el: ta, get: () => ta.value, set: (s) => { ta.value = s; autosize(); }, refresh: autosize };
}

cells.forEach((cell, i) => {
  const code = cell.querySelector('script[type="text/r"]').textContent.replace(/^\n/, '').replace(/\s+$/, '');
  const bar = document.createElement('div');
  bar.className = 'bar';
  const run = document.createElement('button');
  run.textContent = '▶ 執行';
  run.className = 'run';
  run.disabled = true;
  const reset = document.createElement('button');
  reset.textContent = '還原程式碼';
  reset.className = 'ghost';
  const label = document.createElement('span');
  label.className = 'label';
  label.textContent = cell.dataset.label || `程式格 ${i}`;   // cell 0 is the toolbox, so content cells start at 1
  bar.append(label, run, reset);
  const out = document.createElement('div');
  out.className = 'out';
  const invalidate = () => { for (let j = i; j < cells.length; j++) ran[j] = false; };   // 改了程式，這格與後面都要重跑
  const ed = makeEditor(code, label.textContent, invalidate, () => runUpTo(i));
  cell.append(bar, ed.el, out);
  cell.closest('details')?.addEventListener('toggle', ed.refresh);   // folded cells have no height until opened
  run.addEventListener('click', () => runUpTo(i));
  reset.addEventListener('click', () => {
    ed.set(code);
    invalidate();
  });
  cell._ed = ed;
  cell._out = out;
  cell._run = run;
});

document.fonts?.ready.then(() => cells.forEach((c) => c._ed.refresh()));   // web fonts change line heights

const webR = new WebR();
let shelter;

async function boot() {
  setStatus('正在載入瀏覽器裡的 R（第一次約 10–30 秒）…', 'busy');
  await webR.init();
  shelter = await new webR.Shelter();
  const files = (document.body.dataset.files || '').split(',').map((s) => s.trim()).filter(Boolean);
  for (const f of files) {
    const res = await fetch(`./${f}`, { cache: 'no-store' });   // always the current data files, never a stale cached copy
    if (!res.ok) throw new Error(`讀不到 ${f}（請用 start_lab 啟動本機伺服器，不要直接雙擊 HTML）`);
    const buf = new Uint8Array(await res.arrayBuffer());
    const dir = f.split('/').slice(0, -1).join('/');
    if (dir) await webR.evalRVoid(`dir.create("${dir}", recursive = TRUE, showWarnings = FALSE)`);
    await webR.FS.writeFile(`/home/web_user/${f}`, buf);
  }
  await webR.evalRVoid('setwd("/home/web_user"); options(width = 90, digits = 4)');
  const pkgs = (document.body.dataset.packages || '').split(',').map((s) => s.trim()).filter(Boolean);
  if (pkgs.length) {
    setStatus(`正在安裝 R 套件：${pkgs.join('、')}…`, 'busy');
    await webR.installPackages(pkgs, { quiet: true });
  }
  cells.forEach((c) => (c._run.disabled = false));
  document.getElementById('runall')?.removeAttribute('disabled');
  setStatus('R 已就緒。按 ▶ 執行，或在程式格裡按 Ctrl+Enter。', 'ok');
}

async function runCell(i) {
  const cell = cells[i];
  const out = cell._out;
  out.innerHTML = '';
  out.classList.add('running');
  const t0 = performance.now();
  try {
    const res = await shelter.captureR(cell._ed.get(), {
      withAutoprint: true,
      captureStreams: true,
      captureConditions: false,
      captureGraphics: { width: 900, height: 540, pointsize: 17 },
    });
    const text = res.output.map((o) => o.data).join('\n');
    if (text.trim()) {
      const pre = document.createElement('pre');
      pre.textContent = text;
      if (res.output.some((o) => o.type === 'stderr')) pre.classList.add('has-err');
      out.append(pre);
    }
    for (const img of res.images) {
      const canvas = document.createElement('canvas');
      canvas.width = img.width;
      canvas.height = img.height;
      canvas.getContext('2d').drawImage(img, 0, 0);
      out.append(canvas);
    }
    ran[i] = true;
  } catch (err) {
    const pre = document.createElement('pre');
    pre.className = 'error';
    pre.textContent = `錯誤：${err.message}`;
    out.append(pre);
    throw err;
  } finally {
    await shelter.purge();
    out.classList.remove('running');
    const t = document.createElement('div');
    t.className = 'timing';
    t.textContent = `${((performance.now() - t0) / 1000).toFixed(1)} 秒`;
    out.append(t);
  }
}

async function runUpTo(i) {
  if (busy) return;
  busy = true;
  cells.forEach((c) => (c._run.disabled = true));
  try {
    for (let j = 0; j <= i; j++) {
      if (j < i && ran[j]) continue;
      setStatus(`正在執行${cells[j].dataset.label || `程式格 ${j}`}…`, 'busy');
      await runCell(j);
    }
    setStatus('完成。可以改程式碼再跑一次。', 'ok');
  } catch {
    setStatus('有程式格出錯了，看紅色訊息。', 'err');
  } finally {
    busy = false;
    cells.forEach((c) => (c._run.disabled = false));
  }
}

document.getElementById('runall')?.addEventListener('click', () => runUpTo(cells.length - 1));

boot().catch((e) => setStatus(`webR 載入失敗：${e.message}`, 'err'));
