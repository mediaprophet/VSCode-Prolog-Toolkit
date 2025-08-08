import * as vscode from 'vscode';
import { QueryHistoryOrchestrator } from '../src/features/queryHistoryManager/QueryHistoryOrchestrator';

export class QueryHistoryWebviewProvider implements vscode.WebviewViewProvider {
  public static readonly viewType = 'prologQueryHistory';
  private _view?: vscode.WebviewView;
  private orchestrator: QueryHistoryOrchestrator;

  constructor(private readonly _extensionUri: vscode.Uri, orchestrator: QueryHistoryOrchestrator) {
    this.orchestrator = orchestrator;
  }

  public resolveWebviewView(
    webviewView: vscode.WebviewView,
    context: vscode.WebviewViewResolveContext,
    _token: vscode.CancellationToken
  ) {
    this._view = webviewView;
    webviewView.webview.options = {
      enableScripts: true,
      localResourceRoots: [this._extensionUri],
    };
    webviewView.webview.html = this._getHtmlForWebview(webviewView.webview);
    webviewView.webview.onDidReceiveMessage(async message => {
      switch (message.type) {
        case 'getHistory': {
          const history = await this.orchestrator.getHistory(message.filter || {});
          this._postMessage({ type: 'history', data: history });
          break;
        }
        case 'getTags': {
          const tags = await this.orchestrator.getAllTags();
          this._postMessage({ type: 'tags', data: tags });
          break;
        }
        case 'addTag': {
          await this.orchestrator.addTags(message.queryId, [message.tag]);
          this._refresh();
          break;
        }
        case 'removeTag': {
          await this.orchestrator.removeTags(message.queryId, [message.tag]);
          this._refresh();
          break;
        }
        case 'getStats': {
          const stats = await this.orchestrator.getStatistics();
          this._postMessage({ type: 'stats', data: stats });
          break;
        }
      }
    });
    this._refresh();
  }

  private _refresh() {
    this._view?.webview.postMessage({ type: 'refresh' });
  }

  private _postMessage(msg: any) {
    this._view?.webview.postMessage(msg);
  }

  private _getHtmlForWebview(webview: vscode.Webview): string {
    // Advanced UI: search/filter, tag editing, analytics charts
    const chartJsCdn = 'https://cdn.jsdelivr.net/npm/chart.js';
    // Use backslash-escaped backticks and ${} in the HTML/JS so TypeScript does not parse them
    return [
      '<!DOCTYPE html>',
      '<html lang="en">',
      '<head>',
      '  <meta charset="UTF-8">',
      '  <meta name="viewport" content="width=device-width, initial-scale=1.0">',
      '  <title>Prolog Query History</title>',
      `  <script src="${chartJsCdn}"></script>`,
      '  <style>',
      '    body { font-family: var(--vscode-font-family); margin: 0; padding: 0; background: var(--vscode-editor-background); color: var(--vscode-editor-foreground); }',
      '    #container { margin: 1em; }',
      '    #searchBar { margin-bottom: 1em; }',
      '    #historyTable { width: 100%; border-collapse: collapse; margin-bottom: 1em; }',
      '    #historyTable th, #historyTable td { border: 1px solid #ccc; padding: 4px 8px; }',
      '    #historyTable th { background: var(--vscode-sideBar-background); }',
      '    .tag { display: inline-block; background: #007acc; color: #fff; border-radius: 3px; padding: 2px 6px; margin: 0 2px; font-size: 0.9em; cursor: pointer; }',
      '    .tag-remove { margin-left: 4px; color: #ff5555; cursor: pointer; }',
      '    .tag-add { color: #007acc; cursor: pointer; margin-left: 4px; }',
      '    #tagsList { margin-bottom: 1em; }',
      '    #charts { display: flex; flex-wrap: wrap; gap: 2em; }',
      '    .chart-container { flex: 1 1 300px; min-width: 300px; }',
      '  </style>',
      '</head>',
      '<body>',
      '  <div id="container">',
      '    <h2>Query History</h2>',
      '    <div id="searchBar">',
      '      <input id="searchInput" type="text" placeholder="Search by command, user, tag..." style="width: 200px;" />',
      '      <select id="statusFilter">',
      '        <option value="">All Statuses</option>',
      '        <option value="pending">Pending</option>',
      '        <option value="running">Running</option>',
      '        <option value="completed">Completed</option>',
      '        <option value="error">Error</option>',
      '        <option value="cancelled">Cancelled</option>',
      '        <option value="timeout">Timeout</option>',
      '      </select>',
      '      <button id="searchBtn">Search</button>',
      '      <button id="clearBtn">Clear</button>',
      '    </div>',
      '    <table id="historyTable">',
      '      <thead>',
      '        <tr>',
      '          <th>Time</th>',
      '          <th>Command</th>',
      '          <th>Status</th>',
      '          <th>User</th>',
      '          <th>Session</th>',
      '          <th>Source</th>',
      '          <th>CPU (%)</th>',
      '          <th>Mem (MB)</th>',
      '          <th>Artifacts</th>',
      '          <th>Tags</th>',
      '          <th>Actions</th>',
      '        </tr>',
      '      </thead>',
      '      <tbody id="historyBody"></tbody>',
      '    </table>',
      '    <h3>All Tags</h3>',
      '    <div id="tagsList"></div>',
      '    <h3>Analytics</h3>',
      '    <div id="charts">',
      '      <div class="chart-container">',
      '        <canvas id="statusChart"></canvas>',
      '      </div>',
      '      <div class="chart-container">',
      '        <canvas id="durationChart"></canvas>',
      '      </div>',
      '      <div class="chart-container">',
      '        <canvas id="rateChart"></canvas>',
      '      </div>',
      '    </div>',
      '  </div>',
      '  <script>',
      '    const vscode = acquireVsCodeApi();',
      '    let allHistory = [];',
      '    let allTags = [];',
      '    let stats = {};',
      '    function refresh() {',
      '      vscode.postMessage({ type: "getHistory" });',
      '      vscode.postMessage({ type: "getTags" });',
      '      vscode.postMessage({ type: "getStats" });',
      '    }',
      '    function renderHistory(history) {',
      '      allHistory = history;',
      '      const tbody = document.getElementById("historyBody");',
      '      tbody.innerHTML = "";',
      '      for (const entry of history) {',
      '        const tr = document.createElement("tr");',
      '        let tagsHtml = (entry.metadata && entry.metadata.tags ? entry.metadata.tags : []).map(tag => `<span class=\'tag\' data-id=\'${entry.id}\' data-tag=\'${tag}\'>${tag}<span class=\'tag-remove\' title=\'Remove\'>&times;</span></span>`).join("");',
      '        let artifactsHtml = (entry.metadata && entry.metadata.artifacts ? entry.metadata.artifacts : []).map(a => `<span title=\'${a.path}\'>${a.name || a.path}</span>`).join(", ");',
      '        let cpu = entry.metadata && entry.metadata.resourceUsage && entry.metadata.resourceUsage.cpuPercent ? entry.metadata.resourceUsage.cpuPercent : "";',
      '        let mem = entry.metadata && entry.metadata.resourceUsage && entry.metadata.resourceUsage.memoryMB ? entry.metadata.resourceUsage.memoryMB : "";',
      '        tr.innerHTML = `',
      '          <td>${new Date(entry.startTime).toLocaleString()}</td>',
      '          <td>${entry.cmd}</td>',
      '          <td>${entry.status}</td>',
      '          <td>${entry.metadata && entry.metadata.userAgent ? entry.metadata.userAgent : ""}</td>',
      '          <td>${entry.metadata && entry.metadata.sessionId ? entry.metadata.sessionId : ""}</td>',
      '          <td>${entry.metadata && entry.metadata.source ? entry.metadata.source : ""}</td>',
      '          <td>${cpu}</td>',
      '          <td>${mem}</td>',
      '          <td>${artifactsHtml}</td>',
      '          <td>${tagsHtml}</td>',
      '          <td><span class=\'tag-add\' data-id=\'${entry.id}\' title=\'Add tag\'>+</span></td>',
      '        `;',
      '        tbody.appendChild(tr);',
      '      }',
      '    }',
      '    function renderTags(tags) {',
      '      allTags = tags;',
      '      const tagsDiv = document.getElementById("tagsList");',
      '      tagsDiv.innerHTML = tags.map(tag => `<span class=\'tag\'>${tag}</span>`).join(" ");',
      '    }',
      '    function renderCharts(stats) {',
      '      // Status chart',
      '      const statusCtx = document.getElementById("statusChart").getContext("2d");',
      '      new Chart(statusCtx, {',
      '        type: "pie",',
      '        data: {',
      '          labels: Object.keys(stats.queriesByStatus || {}),',
      '          datasets: [{',
      '            data: Object.values(stats.queriesByStatus || {}),',
      '            backgroundColor: ["#007acc", "#28a745", "#dc3545", "#ffc107", "#6c757d", "#17a2b8"],',
      '          }],',
      '        },',
      '        options: { plugins: { legend: { display: true } }, responsive: true }',
      '      });',
      '      // Duration chart',
      '      const durationCtx = document.getElementById("durationChart").getContext("2d");',
      '      new Chart(durationCtx, {',
      '        type: "bar",',
      '        data: {',
      '          labels: (stats.dailyStats || []).map(d => d.date),',
      '          datasets: [{',
      '            label: "Avg Duration (ms)",',
      '            data: (stats.dailyStats || []).map(d => d.avgDuration),',
      '            backgroundColor: "#007acc",',
      '          }],',
      '        },',
      '        options: { plugins: { legend: { display: false } }, responsive: true }',
      '      });',
      '      // Rate chart',
      '      const rateCtx = document.getElementById("rateChart").getContext("2d");',
      '      new Chart(rateCtx, {',
      '        type: "line",',
      '        data: {',
      '          labels: (stats.dailyStats || []).map(d => d.date),',
      '          datasets: [{',
      '            label: "Queries per Day",',
      '            data: (stats.dailyStats || []).map(d => d.count),',
      '            borderColor: "#28a745",',
      '            backgroundColor: "rgba(40,167,69,0.2)",',
      '            fill: true,',
      '          }],',
      '        },',
      '        options: { plugins: { legend: { display: true } }, responsive: true }',
      '      });',
      '    }',
      '    function filterHistory() {',
      '      const search = document.getElementById("searchInput").value.toLowerCase();',
      '      const status = document.getElementById("statusFilter").value;',
      '      let filtered = allHistory;',
      '      if (search) {',
      '        filtered = filtered.filter(e =>',
      '          (e.cmd && e.cmd.toLowerCase().includes(search)) ||',
      '          (e.metadata && e.metadata.userAgent && e.metadata.userAgent.toLowerCase().includes(search)) ||',
      '          (e.metadata && e.metadata.tags && e.metadata.tags.some(t => t.toLowerCase().includes(search)))',
      '        );',
      '      }',
      '      if (status) {',
      '        filtered = filtered.filter(e => e.status === status);',
      '      }',
      '      renderHistory(filtered);',
      '    }',
      '    document.addEventListener("click", e => {',
      '      const target = e.target;',
      '      if (target.classList.contains("tag-remove")) {',
      '        const tag = target.parentElement.getAttribute("data-tag");',
      '        const id = target.parentElement.getAttribute("data-id");',
      '        vscode.postMessage({ type: "removeTag", queryId: id, tag });',
      '      } else if (target.classList.contains("tag-add")) {',
      '        const id = target.getAttribute("data-id");',
      '        const tag = prompt("Enter new tag:");',
      '        if (tag) vscode.postMessage({ type: "addTag", queryId: id, tag });',
      '      }',
      '    });',
      '    document.getElementById("searchBtn").onclick = filterHistory;',
      '    document.getElementById("clearBtn").onclick = () => {',
      '      document.getElementById("searchInput").value = "";',
      '      document.getElementById("statusFilter").value = "";',
      '      renderHistory(allHistory);',
      '    };',
      '    window.addEventListener("message", event => {',
      '      const msg = event.data;',
      '      if (msg.type === "history") {',
      '        renderHistory(msg.data);',
      '      } else if (msg.type === "tags") {',
      '        renderTags(msg.data);',
      '      } else if (msg.type === "stats") {',
      '        stats = msg.data;',
      '        renderCharts(stats);',
      '      } else if (msg.type === "refresh") {',
      '        refresh();',
      '      }',
      '    });',
      '    refresh();',
      '  <\/script>',
      '</body>',
      '</html>'
    ].join('\n');
  }
}
