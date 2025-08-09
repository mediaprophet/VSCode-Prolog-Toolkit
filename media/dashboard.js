// Prolog Dashboard Webview Script
(function () {
  const vscode = acquireVsCodeApi();

  // Event listeners
  document.getElementById('refreshBtn').addEventListener('click', () => {
    vscode.postMessage({ type: 'refreshStatus' });
  });

  document.getElementById('executeBtn').addEventListener('click', () => {
    const query = document.getElementById('queryInput').value.trim();
    if (query) {
      vscode.postMessage({ type: 'executeQuery', query: query });
      document.getElementById('queryInput').value = '';
    }
  });

  document.getElementById('queryInput').addEventListener('keypress', (e) => {
    if (e.key === 'Enter') {
      document.getElementById('executeBtn').click();
    }
  });

  document.getElementById('setupWizardBtn').addEventListener('click', () => {
    vscode.postMessage({ type: 'setupWizard' });
  });

  document.getElementById('openSettingsBtn').addEventListener('click', () => {
    vscode.postMessage({ type: 'openSettings' });
  });

  document.getElementById('newFileBtn').addEventListener('click', () => {
    vscode.postMessage({ type: 'newFile' });
  });

  document.getElementById('clearHistoryBtn').addEventListener('click', () => {
    vscode.postMessage({ type: 'clearHistory' });
  });

  // Example query buttons
  document.querySelectorAll('.example-btn').forEach(btn => {
    btn.addEventListener('click', () => {
      const query = btn.getAttribute('data-query');
      document.getElementById('queryInput').value = query;
    });
  });

  // Handle messages from extension
  window.addEventListener('message', event => {
    const message = event.data;
    switch (message.type) {
      case 'statusUpdate':
        updateDashboard(message.data);
        break;
      case 'error':
        showError(message.message);
        break;
    }
  });

  function updateDashboard(data) {
    // Update backend server status
    const backendStatus = document.getElementById('backendStatus');
    const backendIndicator = backendStatus.querySelector('.status-indicator');
    const backendDetails = document.getElementById('backendDetails');
    if (data.backend) {
      let stateIcon = '❔', stateText = 'Unknown', handshakeIcon = '❔', handshakeText = 'Unknown', errorText = '-';
      switch (data.backend.state) {
        case 'not_started':
          stateIcon = '⏹️'; stateText = 'Not Started'; break;
        case 'starting':
          stateIcon = '🔄'; stateText = 'Starting...'; break;
        case 'running':
          stateIcon = '✅'; stateText = 'Running'; break;
        case 'error':
          stateIcon = '❌'; stateText = 'Error'; break;
      }
      handshakeIcon = data.backend.handshakeOk ? '🤝' : '❌';
      handshakeText = data.backend.handshakeOk ? 'Success' : 'Failed';
      errorText = data.backend.lastError ? escapeHtml(data.backend.lastError) : '-';

      backendIndicator.className = 'status-indicator ' + (data.backend.state === 'running' && data.backend.handshakeOk ? 'success' : (data.backend.state === 'error' ? 'error' : 'loading'));
      backendIndicator.innerHTML = `<span class="status-icon">${stateIcon}</span><span class="status-text">${stateText}</span>`;
      document.getElementById('backendState').textContent = stateText;
      document.getElementById('backendHandshake').innerHTML = `${handshakeIcon} ${handshakeText}`;
      document.getElementById('backendLastError').textContent = errorText;
      backendDetails.style.display = 'block';
    } else {
      backendIndicator.className = 'status-indicator loading';
      backendIndicator.innerHTML = '<span class="status-icon">🔄</span><span class="status-text">Checking backend server...</span>';
      backendDetails.style.display = 'none';
    }

    // Update installation status
    const installationStatus = document.getElementById('installationStatus');
    const statusIndicator = installationStatus.querySelector('.status-indicator');
    const statusDetails = document.getElementById('installationDetails');

    if (data.installation.isInstalled) {
      statusIndicator.className = 'status-indicator success';
      statusIndicator.innerHTML = '<span class="status-icon">✅</span><span class="status-text">SWI-Prolog Installed</span>';
      document.getElementById('installationVersion').textContent = data.installation.version;
      document.getElementById('installationPath').textContent = data.installation.path;
      statusDetails.style.display = 'block';
    } else {
      statusIndicator.className = 'status-indicator error';
      statusIndicator.innerHTML = '<span class="status-icon">❌</span><span class="status-text">SWI-Prolog Not Found</span>';
      statusDetails.style.display = 'none';
    }

    // Update recent queries
    const queriesContainer = document.getElementById('recentQueries');
    if (data.queries.length === 0) {
      queriesContainer.innerHTML = '<div class="empty-message">No recent queries</div>';
    } else {
      queriesContainer.innerHTML = data.queries.map(query =>
        '<div class="query-item">' +
        '<div class="query-text">' + escapeHtml(query.query) + '</div>' +
        '<div class="query-status ' + (query.success ? 'success' : 'error') + '">' +
        (query.success ? '✅' : '❌') +
        '</div>' +
        '</div>'
      ).join('');
    }

    // Update workspace files
    const filesContainer = document.getElementById('workspaceFiles');
    if (!data.workspace.hasWorkspace) {
      filesContainer.innerHTML = '<div class="empty-message">No workspace open</div>';
    } else if (data.files.length === 0) {
      filesContainer.innerHTML = '<div class="empty-message">No Prolog files found</div>';
    } else {
      filesContainer.innerHTML = data.files.map(file =>
        '<div class="file-item" onclick="vscode.postMessage({type: \'openFile\', filePath: \'" + file.path + "\'})">' +
        '<span class="file-icon">📄</span>' +
        '<span class="file-name">' + escapeHtml(file.name) + '</span>' +
        '<span class="file-path">' + escapeHtml(file.relativePath) + '</span>' +
        '</div>'
      ).join('');
    }

    // Update debug status
    const debugStatus = document.getElementById('debugStatus');
    const debugIndicator = debugStatus.querySelector('.status-indicator');
    if (data.debugging.isActive) {
      debugIndicator.innerHTML = '<span class="status-icon">🐛</span><span class="status-text">Debugging: ' + escapeHtml(data.debugging.sessionName) + '</span>';
    } else {
      debugIndicator.innerHTML = '<span class="status-icon">⏸️</span><span class="status-text">No active debug session</span>';
    }
  }

  function showError(message) {
    // Simple error display - could be enhanced with a proper notification system
    console.error('Dashboard error:', message);
  }

  function escapeHtml(text) {
    const div = document.createElement('div');
    div.textContent = text;
    return div.innerHTML;
  }

  // Request initial status
  vscode.postMessage({ type: 'refreshStatus' });
})();
