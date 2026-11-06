"""Build the Pokemon-CP labs from one source per lab.

    python src/build.py

src/labs/NN_name.md      lab source (text + R cells), see the syntax note below
src/toolbox/*.R          shared R functions; each lab lists the modules it needs
-> NN_name.html          webR page (toolbox embedded in the page, no R/ folder needed)
-> colab/NN_name.ipynb   Colab notebook (R runtime; data downloaded from DATA_BASE)

Source syntax
  front matter between --- lines: id, title, nav, lead, toolbox, files, packages, time
  ## NN Title | slide note        section heading
  ```r ... ```                   R cell
  ::: look / ::: think Question / ::: note / ::: warn / ::: punch   ...   :::
  - bullet, | pipe | table |, blank line = new paragraph; **bold**, `code`, raw HTML allowed
"""
import html, io, json, os, re

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
SRC = os.path.join(ROOT, 'src')
# GitHub "owner/repo" that hosts this folder: Colab downloads data from it, pages link to Colab through it
REPO = os.environ.get('POKEMON_CP_REPO', 'kiwi710680/Pokemon-CP')
DATA_BASE = f'https://raw.githubusercontent.com/{REPO}/main/'
SITE = 'https://{}.github.io/{}/'.format(*REPO.split('/'))
colab_url = lambda lab_id: f'https://colab.research.google.com/github/{REPO}/blob/main/colab/{lab_id}.ipynb'
# CodeMirror 6 for the code cells (web/runner.js). esm.sh "/*pkg" leaves every dependency as a bare import,
# so this map is the one place each package is resolved: one copy of each, all versions pinned.
CODEMIRROR = {
    '@codemirror/state': '@codemirror/state@6.4.1',
    '@codemirror/view': '@codemirror/view@6.26.3',
    '@codemirror/language': '@codemirror/language@6.10.1',
    '@codemirror/commands': '@codemirror/commands@6.5.0',
    '@codemirror/legacy-modes/mode/r': '@codemirror/legacy-modes@6.4.0/mode/r',
    '@lezer/common': '@lezer/common@1.2.1',
    '@lezer/highlight': '@lezer/highlight@1.2.0',
    '@lezer/lr': '@lezer/lr@1.4.0',
    'style-mod': 'style-mod@4.1.2',
    'w3c-keyname': 'w3c-keyname@2.2.8',
}
IMPORTMAP = ('<script type="importmap">\n'
             + json.dumps({'imports': {k: f'https://esm.sh/*{v}' for k, v in CODEMIRROR.items()}}, indent=1)
             + '\n</script>')


def split_title(title):
    """'Lab 2　Candy (I): long title' -> ('Lab 2　Candy (I)', 'long title'); no colon -> ('Lab 1', rest)"""
    if '：' in title:
        k, _, h = title.partition('：')
        return k, h
    k, _, h = title.partition('　')
    return k, h


def parse(path):
    text = io.open(path, encoding='utf-8').read().replace('\r\n', '\n')
    _, fm, body = text.split('---\n', 2)
    meta = {}
    for line in fm.strip().split('\n'):
        k, v = line.split(':', 1)
        meta[k.strip()] = v.strip()
    for k in ('toolbox', 'files', 'packages'):
        meta[k] = [s.strip() for s in meta.get(k, '').split(',') if s.strip()]
    blocks, lines, i = [], body.split('\n'), 0
    para = []

    def flush():
        if para:
            blocks.append(('text', '\n'.join(para)))
            para.clear()
    while i < len(lines):
        ln = lines[i]
        if ln.startswith('```r'):
            flush(); j = i + 1
            while not lines[j].startswith('```'): j += 1
            blocks.append(('code', '\n'.join(lines[i + 1:j]).strip('\n'))); i = j + 1; continue
        if ln.startswith(':::') and ln.strip() != ':::':
            flush(); kind, _, arg = ln[3:].strip().partition(' '); j = i + 1
            while lines[j].strip() != ':::': j += 1
            blocks.append((kind, (arg, '\n'.join(lines[i + 1:j]).strip()))); i = j + 1; continue
        if ln.startswith('## '):
            flush(); head = ln[3:]
            num, _, rest = head.partition(' ')
            title, _, slide = rest.partition(' | ')
            blocks.append(('h2', (num, title.strip(), slide.strip()))); i += 1; continue
        if not ln.strip():
            flush(); i += 1; continue
        para.append(ln); i += 1
    flush()
    return meta, blocks


def inline(s):
    s = re.sub(r'\*\*(.+?)\*\*', r'<b>\1</b>', s)
    s = re.sub(r'`([^`]+)`', lambda m: '<code>' + m.group(1).replace('<', '&lt;') + '</code>', s)
    return s


def text_html(t):
    """paragraphs, bullet lists and pipe tables -> HTML"""
    out, lines = [], t.split('\n')
    if all(l.startswith('|') for l in lines):
        rows = [[c.strip() for c in l.strip('|').split('|')] for l in lines if not re.match(r'^\|[\s:|-]+\|$', l)]
        cls = ' class="map"' if rows and rows[0] and rows[0][0] in ('格', '投影片') else ''
        h = ''.join(f'<th>{inline(c)}</th>' for c in rows[0])
        b = ''.join('<tr>' + ''.join(f'<td>{inline(c)}</td>' for c in r) + '</tr>' for r in rows[1:])
        return f'<table{cls}><tr>{h}</tr>{b}</table>'
    if all(l.startswith('- ') for l in lines):
        return '<ul>' + ''.join(f'<li>{inline(l[2:])}</li>' for l in lines) + '</ul>'
    return '<p>' + inline(' '.join(lines)) + '</p>'


def toolbox_code(meta):
    parts = [io.open(os.path.join(SRC, 'toolbox', m + '.R'), encoding='utf-8').read().strip() for m in meta['toolbox']]
    return '\n\n'.join(parts)


def nav(labs, cur):
    return ''.join(f'<a href="{m["id"]}.html"' + (' aria-current="page"' if m['id'] == cur else '') + f'>{m["nav"]}</a>' for m in labs)


def build_html(meta, blocks, labs):
    tb = toolbox_code(meta)
    kicker, h1 = split_title(meta['title'])
    i = [m['id'] for m in labs].index(meta['id'])
    prev_l = f'<a href="{labs[i-1]["id"]}.html">← {labs[i-1]["nav"]}</a>' if i > 0 else '<a href="index.html">← 首頁</a>'
    next_l = f'<a href="{labs[i+1]["id"]}.html">{labs[i+1]["nav"]} →</a>' if i + 1 < len(labs) else '<a href="index.html">回首頁 →</a>'
    out = [f'''<!doctype html>
<html lang="zh-Hant">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>{html.escape(meta["title"])}</title>
<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
<link href="https://fonts.googleapis.com/css2?family=JetBrains+Mono:wght@400;600&family=Noto+Sans+TC:wght@400;700;900&display=swap" rel="stylesheet">
<link rel="stylesheet" href="web/style.css?v=8">
{IMPORTMAP}
</head>
<body data-files="{",".join(meta["files"])}" data-packages="{",".join(meta["packages"])}">
<header class="top"><div class="wrap">
  <a class="home" href="index.html">Pokémon-CP Lab</a>
  <nav class="labs">{nav(labs, meta["id"])}</nav>
  <span id="status">準備中…</span>
</div></header>
<main>
<p class="kicker">{inline(kicker)}</p>
<h1>{inline(h1)}</h1>
<p class="lead">{inline(meta["lead"])}</p>
<div class="actions"><button id="runall" disabled>▶ 全部執行</button><span class="time">{meta.get("time", "")}</span>
<a class="colab" href="{colab_url(meta["id"])}" target="_blank" rel="noopener">在 Colab 開啟</a>
<a class="dl" href="colab/{meta["id"]}.ipynb" download>下載 .ipynb</a></div>
<details class="toolbox"><summary>工具箱：這一頁用到的 R 函式（會自動先執行；點開可以看每一行）</summary>
<div class="cell" data-label="工具箱"><script type="text/r">
{tb}
</script></div></details>
''']
    for kind, val in blocks:
        if kind == 'text':
            out.append(text_html(val))
        elif kind == 'code':
            out.append(f'<div class="cell"><script type="text/r">\n{val}\n</script></div>')
        elif kind == 'h2':
            num, title, slide = val
            s = f'<span class="slide">{inline(slide)}</span>' if slide else ''
            out.append(f'<h2><span class="n">{num}</span>{inline(title)}{s}</h2>')
        else:
            arg, body = val
            inner = '\n'.join(text_html(p) for p in re.split(r'\n\s*\n', body))
            if kind == 'look':
                out.append(f'<div class="look"><b>看什麼：</b>{inner}</div>')
            elif kind == 'think':
                out.append(f'<details class="think"><summary>想一想：{inline(arg)}</summary>{inner}</details>')
            elif kind == 'punch':
                out.append(f'<p class="punch">{inline(body)}</p>')
            else:  # note / warn
                out.append(f'<div class="note {kind}">{inner}</div>')
    out.append(f'<nav class="pager">{prev_l}{next_l}</nav>')
    out.append('</main>\n<script type="module" src="web/runner.js?v=8"></script>\n</body>\n</html>\n')
    return '\n'.join(out)


def md_inline(s):
    return s  # Colab markdown understands **bold**, `code` and simple inline HTML such as <sub>


def text_md(t):
    lines = t.split('\n')
    if all(l.startswith('|') for l in lines):
        rows = [l for l in lines if not re.match(r'^\|[\s:|-]+\|$', l)]
        ncol = rows[0].count('|') - 1
        return '\n'.join([rows[0], '|' + '---|' * ncol] + rows[1:])
    return '\n'.join(md_inline(l) for l in lines) if all(l.startswith('- ') for l in lines) else md_inline(' '.join(lines))


def setup_code(meta):
    files = ', '.join(f'"{f}"' for f in meta['files']) or ''
    pk = ', '.join(f'"{p}"' for p in meta['packages'])
    code = [f'# Setup: download the data used by this lab (from {DATA_BASE}) and set plot size',
            f'BASE <- "{DATA_BASE}"',
            f'files <- c({files})',
            'for (f in files) if (!file.exists(f)) {',
            '  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)',
            '  for (try in 1:3) {                                   # retry: a single network hiccup should not stop the lab',
            '    ok <- tryCatch(download.file(paste0(BASE, f), f, quiet = TRUE, mode = "wb") == 0, error = function(e) FALSE)',
            '    if (ok) break else Sys.sleep(2)',
            '  }',
            '  if (!ok) stop("could not download ", f, " - check the internet connection and run this cell again")',
            '}']
    if pk:
        code += [f'for (p in c({pk})) if (!requireNamespace(p, quietly = TRUE)) install.packages(p, quiet = TRUE)']
    code += ['options(repr.plot.width = 12, repr.plot.height = 7, repr.plot.res = 100, width = 110, digits = 4)',
             'cat(if (length(files)) paste("ready:", length(files), "data file(s)") else "ready: this lab simulates its own data", "\\n")']
    return '\n'.join(code)


def build_ipynb(meta, blocks):
    cells = []
    md = lambda s: cells.append({'cell_type': 'markdown', 'metadata': {}, 'source': s.strip('\n').splitlines(True)})
    code = lambda s: cells.append({'cell_type': 'code', 'metadata': {}, 'execution_count': None, 'outputs': [],
                                   'source': s.strip('\n').splitlines(True)})
    md(f'# {meta["title"]}\n\n{meta["lead"]}\n\n'
       '> **怎麼跑：** 這本筆記本用 **R** 執行（Colab 會自動選 R runtime；如果右上角不是 R，到「執行階段 → 變更執行階段類型」選 R）。'
       '從上往下依序執行：第一格下載資料、第二格載入工具箱，之後每一格都可以改了再跑。'
       '想一次跑完：「執行階段 → 全部執行」。\n>\n'
       f'> 網頁版（不用登入、直接在瀏覽器跑）：[{SITE}{meta["id"]}.html]({SITE}{meta["id"]}.html)')
    code(setup_code(meta))
    md('## 工具箱\n\n這一頁用到的 R 函式都在下面這一格：估計器、資料讀取、畫圖小工具。先執行它；想知道某個函式做什麼，回來這裡找。')
    code(toolbox_code(meta))
    for kind, val in blocks:
        if kind == 'text':
            md(text_md(val))
        elif kind == 'code':
            code(val)
        elif kind == 'h2':
            num, title, slide = val
            md(f'## {num}　{title}' + (f'　<small>（{slide}）</small>' if slide else ''))
        else:
            arg, body = val
            paras = [text_md(p) for p in re.split(r'\n\s*\n', body)]
            if kind == 'look':
                q = '\n>\n'.join('\n'.join('> ' + l for l in p.split('\n')) for p in paras)
                md('> **看什麼：**\n>\n' + q)
            elif kind == 'think':
                md(f'<details><summary><b>想一想：{arg}</b></summary>\n\n' + '\n\n'.join(paras) + '\n\n</details>')
            elif kind == 'punch':
                md(f'### {body}')
            else:
                md('\n'.join('> ' + l for l in '\n\n'.join(paras).split('\n')))
    nb = {'cells': cells, 'nbformat': 4, 'nbformat_minor': 5,
          'metadata': {'kernelspec': {'name': 'ir', 'display_name': 'R', 'language': 'R'},
                       'language_info': {'name': 'R'},
                       'colab': {'provenance': [], 'toc_visible': True}}}
    return json.dumps(nb, ensure_ascii=False, indent=1)


def main():
    srcs = sorted(f for f in os.listdir(os.path.join(SRC, 'labs')) if f.endswith('.md'))
    labs = [parse(os.path.join(SRC, 'labs', f)) for f in srcs]
    metas = [m for m, _ in labs]
    for meta, blocks in labs:
        io.open(os.path.join(ROOT, meta['id'] + '.html'), 'w', encoding='utf-8').write(build_html(meta, blocks, metas))
        io.open(os.path.join(ROOT, 'colab', meta['id'] + '.ipynb'), 'w', encoding='utf-8').write(build_ipynb(meta, blocks))
        n = sum(1 for k, _ in blocks if k == 'code')
        print(f'{meta["id"]}: {n} R cells')


if __name__ == '__main__':
    main()
