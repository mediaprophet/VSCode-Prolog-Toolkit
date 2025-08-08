// Settings Webview JavaScript
(function () {
  const vscode = acquireVsCodeApi();
  let currentSettings = {};
  let searchTimeout = null;

  // Initialize when DOM is loaded
  document.addEventListener('DOMContentLoaded', function () {
    initializeEventListeners();
    requestCurrentSettings();
  });

  // Initialize all event listeners
  function initializeEventListeners() {
    // Action buttons
    document.getElementById('searchBtn').addEventListener('click', toggleSearch);
    document.getElementById('exportBtn').addEventListener('click', exportSettings);
    document.getElementById('importBtn').addEventListener('click', importSettings);
    document.getElementById('resetBtn').addEventListener('click', resetSettings);

    document.getElementById('viewLogsBtn').addEventListener('click', () => {
      vscode.postMessage({ type: 'viewLogs' });
    });
    document.getElementById('docsBtn').addEventListener('click', () => {
      vscode.postMessage({ type: 'openDocs' });
    });
    // Save button
    document.getElementById('saveBtn').addEventListener('click', saveAllSettings);
    // Collect all settings from the form and send to extension
    function saveAllSettings() {
      const settings = {};
      document.querySelectorAll('input, select, textarea').forEach(control => {
        const key = control.getAttribute('data-key');
        if (!key) return;
        let value;
        if (control.type === 'checkbox') {
          value = control.checked;
        } else if (control.type === 'number') {
          value = control.valueAsNumber;
        } else {
          value = control.value;
        }
        settings[key] = value;
      });
      vscode.postMessage({ type: 'saveAllSettings', settings });
      announceToScreenReader('Settings saved');
    }

    // Search functionality
    const searchInput = document.getElementById('searchInput');
    const clearSearch = document.getElementById('clearSearch');

    searchInput.addEventListener('input', handleSearch);
    clearSearch.addEventListener('click', clearSearchInput);

    // Section toggles
    document.querySelectorAll('.section-title').forEach(title => {
      title.addEventListener('click', toggleSection);
    });

    // Form controls
    document.querySelectorAll('input, select, textarea').forEach(control => {
      if (control.type === 'checkbox') {
        control.addEventListener('change', handleSettingChange);
      } else {
        control.addEventListener('input', handleSettingChange);
        control.addEventListener('blur', validateSetting);
      }
    });

    // Conditional display handlers
    setupConditionalDisplay();
  }

  // Request current settings from extension
  function requestCurrentSettings() {
    vscode.postMessage({ type: 'getSettings' });
  }

  // Handle messages from extension
  window.addEventListener('message', event => {
    const message = event.data;
    switch (message.type) {
      case 'settingsData':
        currentSettings = message.settings;
        populateSettings(message.settings);
        break;
      case 'validationResult':
        showValidationResult(message.key, message.isValid, message.errorMessage);
        break;
      case 'testResult':
        showTestResult(message);
        break;
    }
  });

  function showTestResult(message) {
    const statusIndicator = document.getElementById('statusIndicator');
    const detailsDiv = document.getElementById('installationDetails');
    const issuesDiv = document.getElementById('installationIssues');
    const issuesList = document.getElementById('issuesList');
    if (!statusIndicator) return;
    if (message.success) {
      statusIndicator.innerHTML = `<span class="status-icon">✅</span> <span class="status-text">${message.message}</span>`;
      if (detailsDiv && message.details) {
        detailsDiv.style.display = 'block';
        document.getElementById('installationPath').textContent = message.details.path || '-';
        document.getElementById('installationVersion').textContent = message.details.version || '-';
        if (issuesDiv) issuesDiv.style.display = 'none';
      }
      // Hide backend error area if present
      const backendErrorArea = document.getElementById('backendErrorArea');
      if (backendErrorArea) backendErrorArea.style.display = 'none';
    } else {
      statusIndicator.innerHTML = `<span class="status-icon">❌</span> <span class="status-text">${message.message}</span>`;
      if (detailsDiv) detailsDiv.style.display = 'block';
      if (issuesDiv && issuesList) {
        issuesDiv.style.display = 'block';
        issuesList.innerHTML = '';
        if (message.details && message.details.issues) {
          message.details.issues.forEach(issue => {
            const li = document.createElement('li');
            li.textContent = issue;
            issuesList.appendChild(li);
          });
        }
        if (message.troubleshooting) {
          message.troubleshooting.forEach(tip => {
            const li = document.createElement('li');
            li.textContent = tip;
            li.style.fontStyle = 'italic';
            issuesList.appendChild(li);
          });
        }
      }
      // Show backend error area if present
      const backendErrorArea = document.getElementById('backendErrorArea');
      if (backendErrorArea) {
        backendErrorArea.style.display = 'block';
        backendErrorArea.querySelector('.backend-error-message').textContent = message.message;
        const troubleshootingList = backendErrorArea.querySelector('.backend-error-troubleshooting');
        troubleshootingList.innerHTML = '';
        if (message.details && message.details.issues) {
          message.details.issues.forEach(issue => {
            const li = document.createElement('li');
            li.textContent = issue;
            troubleshootingList.appendChild(li);
          });
        }
        if (message.troubleshooting) {
          message.troubleshooting.forEach(tip => {
            const li = document.createElement('li');
            li.textContent = tip;
            li.style.fontStyle = 'italic';
            troubleshootingList.appendChild(li);
          });
        }
      }
    }
  }

  // Populate form with current settings
  function populateSettings(settings) {
    // Core settings
    setValue('executablePath', settings.executablePath);
    setValue('dialect', settings.dialect);
    setValue('terminalRuntimeArgs', JSON.stringify(settings.terminal.runtimeArgs, null, 2));

    // Linter settings
    setValue('linterRun', settings.linter.run);
    setValue('linterDelay', settings.linter.delay);
    setValue('linterEnableMsgInOutput', settings.linter.enableMsgInOutput);

    // Formatter settings
    setValue('formatAddSpace', settings.format.addSpace);

    // API Server settings
    setValue('apiServerEnabled', settings.apiServer.enabled);
    setValue('apiServerPort', settings.apiServer.port);
    setValue('apiServerHost', settings.apiServer.host);
    setValue('apiServerCorsOrigins', JSON.stringify(settings.apiServer.corsOrigins, null, 2));
    setValue('apiServerMaxConnections', settings.apiServer.maxConnections);
    setValue('apiServerRequestTimeout', settings.apiServer.requestTimeout);
    setValue('apiServerRateLimitingEnabled', settings.apiServer.rateLimiting.enabled);
    setValue('apiServerRateLimitingRequestsPerMinute', settings.apiServer.rateLimiting.requestsPerMinute);
    setValue('apiServerRateLimitingBurstLimit', settings.apiServer.rateLimiting.burstLimit);
    setValue('apiServerAuthMethod', settings.apiServer.auth.method);
    setValue('apiServerAuthJwtExpiration', settings.apiServer.auth.jwtExpiration);

    // WebSocket Server settings
    setValue('webSocketServerEnabled', settings.webSocketServer.enabled);
    setValue('webSocketServerPort', settings.webSocketServer.port);
    setValue('webSocketServerMaxConnections', settings.webSocketServer.maxConnections);
    setValue('webSocketServerHeartbeatInterval', settings.webSocketServer.heartbeatInterval);

    // Privacy settings
    setValue('telemetryEnabled', settings.telemetry.enabled);

    // Update conditional displays
    updateConditionalDisplay();
  }

  // Set value for form control
  function setValue(id, value) {
    const element = document.getElementById(id);
    if (!element) return;

    if (element.type === 'checkbox') {
      element.checked = Boolean(value);
    } else {
      element.value = value;
    }
  }

  // Handle setting changes
  function handleSettingChange(event) {
    const element = event.target;
    const key = element.dataset.key;
    if (!key) return;

    let value = element.value;

    // Handle different input types
    if (element.type === 'checkbox') {
      value = element.checked;
    } else if (element.type === 'number') {
      value = parseInt(value) || 0;
    } else if (element.id.includes('Args') || element.id.includes('Origins')) {
      // Handle JSON arrays
      try {
        value = JSON.parse(value);
      } catch (e) {
        // Invalid JSON, don't update yet
        return;
      }
    }

    // Update setting
    vscode.postMessage({
      type: 'updateSetting',
      key: key,
      value: value
    });

    // Update conditional displays
    updateConditionalDisplay();
  }

  // Validate setting
  function validateSetting(event) {
    const element = event.target;
    const key = element.dataset.key;
    if (!key) return;

    let value = element.value;
    if (element.type === 'number') {
      value = parseInt(value) || 0;
    }

    vscode.postMessage({
      type: 'validateSetting',
      key: key,
      value: value
    });
  }

  // Show validation result
  function showValidationResult(key, isValid, errorMessage) {
    const element = document.querySelector(`[data-key="${key}"]`);
    if (!element) return;

    const validationDiv = document.getElementById(`${element.id}-validation`);
    if (!validationDiv) return;

    validationDiv.className = 'validation-message';

    if (isValid) {
      validationDiv.classList.add('success');
      validationDiv.textContent = '✓ Valid';
    } else {
      validationDiv.classList.add('error');
      validationDiv.textContent = errorMessage || 'Invalid value';
    }

    // Clear validation message after 3 seconds if valid
    if (isValid) {
      setTimeout(() => {
        validationDiv.style.display = 'none';
      }, 3000);
    }
  }

  // Setup conditional display logic
  function setupConditionalDisplay() {
    // API Server dependencies
    const apiServerEnabled = document.getElementById('apiServerEnabled');
    if (apiServerEnabled) {
      apiServerEnabled.addEventListener('change', updateConditionalDisplay);
    }

    // WebSocket Server dependencies
    const webSocketServerEnabled = document.getElementById('webSocketServerEnabled');
    if (webSocketServerEnabled) {
      webSocketServerEnabled.addEventListener('change', updateConditionalDisplay);
    }

    // Rate limiting dependencies
    const rateLimitingEnabled = document.getElementById('apiServerRateLimitingEnabled');
    if (rateLimitingEnabled) {
      rateLimitingEnabled.addEventListener('change', updateConditionalDisplay);
    }

    // JWT dependencies
    const authMethod = document.getElementById('apiServerAuthMethod');
    if (authMethod) {
      authMethod.addEventListener('change', updateConditionalDisplay);
    }
  }

  // Update conditional display
  function updateConditionalDisplay() {
    // API Server dependencies
    const apiEnabled = document.getElementById('apiServerEnabled')?.checked || false;
    document.querySelectorAll('.api-dependent').forEach(element => {
      element.classList.toggle('disabled', !apiEnabled);
    });

    // WebSocket Server dependencies
    const wsEnabled = document.getElementById('webSocketServerEnabled')?.checked || false;
    document.querySelectorAll('.websocket-dependent').forEach(element => {
      element.classList.toggle('disabled', !wsEnabled);
    });

    // Rate limiting dependencies
    const rateLimitingEnabled = document.getElementById('apiServerRateLimitingEnabled')?.checked || false;
    document.querySelectorAll('.rate-limiting-dependent').forEach(element => {
      element.classList.toggle('disabled', !rateLimitingEnabled);
    });

    // JWT dependencies
    const authMethod = document.getElementById('apiServerAuthMethod')?.value || 'local_only';
    const isJWT = authMethod === 'jwt_token';
    document.querySelectorAll('.jwt-dependent').forEach(element => {
      element.classList.toggle('disabled', !isJWT);
    });
  }

  // Toggle search visibility
  function toggleSearch() {
    const searchContainer = document.getElementById('searchContainer');
    const isVisible = searchContainer.style.display !== 'none';

    searchContainer.style.display = isVisible ? 'none' : 'block';

    if (!isVisible) {
      document.getElementById('searchInput').focus();
    } else {
      clearSearchInput();
    }
  }

  // Handle search input
  function handleSearch(event) {
    const query = event.target.value.toLowerCase().trim();

    // Debounce search
    clearTimeout(searchTimeout);
    searchTimeout = setTimeout(() => {
      performSearch(query);
    }, 300);
  }

  // Perform search
  function performSearch(query) {
    const sections = document.querySelectorAll('.settings-section');

    if (!query) {
      // Show all sections and remove highlights
      sections.forEach(section => {
        section.style.display = 'block';
        removeHighlights(section);
      });
      return;
    }

    sections.forEach(section => {
      const hasMatch = searchInSection(section, query);
      section.style.display = hasMatch ? 'block' : 'none';

      if (hasMatch) {
        // Expand section if it has matches
        const content = section.querySelector('.section-content');
        const toggle = section.querySelector('.section-toggle');
        if (content && toggle) {
          content.classList.remove('collapsed');
          toggle.classList.remove('collapsed');
        }
      }
    });
  }

  // Search within a section
  function searchInSection(section, query) {
    removeHighlights(section);

    const searchableElements = section.querySelectorAll('label, .setting-description, h2, h3');
    let hasMatch = false;

    searchableElements.forEach(element => {
      const text = element.textContent.toLowerCase();
      if (text.includes(query)) {
        hasMatch = true;
        highlightText(element, query);
      }
    });

    return hasMatch;
  }

  // Highlight matching text
  function highlightText(element, query) {
    const text = element.textContent;
    const regex = new RegExp(`(${escapeRegExp(query)})`, 'gi');
    const highlightedText = text.replace(regex, '<span class="search-highlight">$1</span>');
    element.innerHTML = highlightedText;
  }

  // Remove highlights
  function removeHighlights(container) {
    const highlights = container.querySelectorAll('.search-highlight');
    highlights.forEach(highlight => {
      const parent = highlight.parentNode;
      parent.replaceChild(document.createTextNode(highlight.textContent), highlight);
      parent.normalize();
    });
  }

  // Escape regex special characters
  function escapeRegExp(string) {
    return string.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
  }

  // Clear search input
  function clearSearchInput() {
    const searchInput = document.getElementById('searchInput');
    searchInput.value = '';
    performSearch('');
  }

  // Toggle section collapse/expand
  function toggleSection(event) {
    const section = event.currentTarget.closest('.settings-section');
    const content = section.querySelector('.section-content');
    const toggle = section.querySelector('.section-toggle');

    const isCollapsed = content.classList.contains('collapsed');

    content.classList.toggle('collapsed', !isCollapsed);
    toggle.classList.toggle('collapsed', !isCollapsed);

    // Update toggle text
    toggle.textContent = isCollapsed ? '−' : '+';
  }

  // Export settings
  function exportSettings() {
    vscode.postMessage({ type: 'exportSettings' });
  }

  // Import settings
  function importSettings() {
    vscode.postMessage({ type: 'importSettings' });
  }

  // Reset settings
  function resetSettings() {
    const confirmed = confirm(
      'Are you sure you want to reset all Prolog settings to their default values? This action cannot be undone.'
    );

    if (confirmed) {
      vscode.postMessage({ type: 'resetSettings' });
    }
  }

  // Keyboard shortcuts
  document.addEventListener('keydown', function (event) {
    // Ctrl/Cmd + F for search
    if ((event.ctrlKey || event.metaKey) && event.key === 'f') {
      event.preventDefault();
      toggleSearch();
    }

    // Escape to close search
    if (event.key === 'Escape') {
      const searchContainer = document.getElementById('searchContainer');
      if (searchContainer.style.display !== 'none') {
        toggleSearch();
      }
    }
  });

  // Auto-save indication
  let saveTimeout = null;
  function showSaveIndicator() {
    // Could add a subtle save indicator here
    clearTimeout(saveTimeout);
    saveTimeout = setTimeout(() => {
      // Hide save indicator
    }, 2000);
  }

  // Form validation helpers
  function validateJSON(value) {
    try {
      JSON.parse(value);
      return true;
    } catch (e) {
      return false;
    }
  }

  function validatePort(port) {
    const num = parseInt(port);
    return num >= 1024 && num <= 65535;
  }

  function validatePositiveNumber(value) {
    const num = parseInt(value);
    return num > 0;
  }

  // Accessibility improvements
  function announceToScreenReader(message) {
    const announcement = document.createElement('div');
    announcement.setAttribute('aria-live', 'polite');
    announcement.setAttribute('aria-atomic', 'true');
    announcement.style.position = 'absolute';
    announcement.style.left = '-10000px';
    announcement.style.width = '1px';
    announcement.style.height = '1px';
    announcement.style.overflow = 'hidden';
    announcement.textContent = message;

    document.body.appendChild(announcement);

    setTimeout(() => {
      document.body.removeChild(announcement);
    }, 1000);
  }

  // Theme change detection
  const observer = new MutationObserver(function (mutations) {
    mutations.forEach(function (mutation) {
      if (mutation.type === 'attributes' && mutation.attributeName === 'class') {
        // Handle theme changes if needed
      }
    });
  });

  observer.observe(document.body, {
    attributes: true,
    attributeFilter: ['class']
  });

})();