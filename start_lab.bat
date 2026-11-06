@echo off
rem Start a local server and open the lab homepage (needs Python 3)
cd /d "%~dp0"
start "" http://localhost:8767/index.html
python -m http.server 8767
