# Makefile
PORT ?= 8000
INDEX ?= index.html
.PHONY: serve open dev backup stop

serve:
	@nohup python3 -m http.server $(PORT) >server.log 2>&1 & echo $$! > .server.pid
	@echo "Server on http://localhost:$(PORT)/ (pid $$(cat .server.pid))"

open:
	@sleep 0.3; python3 -c 'import webbrowser; webbrowser.open("http://localhost:$(PORT)/$(INDEX)")' >/dev/null 2>&1 &

dev: serve open

backup:
	@git add -A
	@git commit -m 'initial commit' || true
	@git push -u origin main

stop:
	@pid=$$(cat .server.pid 2>/dev/null || true); \
	if [ -n "$$pid" ]; then kill $$pid && rm -f .server.pid && echo "Stopped $$pid"; \
	else echo "No server running."; fi
