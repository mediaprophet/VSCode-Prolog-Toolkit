(function() {
    const vscode = acquireVsCodeApi();
    
    // DOM elements
    let statusElement;
    let refreshButton;
    let backendStatusElement;
    let installationStatusElement;
    let errorContainer;
    
    // Initialize when DOM is loaded
    document.addEventListener('DOMContentLoaded', function() {
        initializeDashboard();
        requestInitialData();
    });
    
    function initializeDashboard() {
        // Get DOM elements
        statusElement = document.getElementById('status');
        refreshButton = document.getElementById('refresh-btn');
        backendStatusElement = document.getElementById('backend-status');
        installationStatusElement = document.getElementById('installation-status');
        errorContainer = document.getElementById('error-container');
        
        // Add event listeners
        if (refreshButton) {
            refreshButton.addEventListener('click', function() {
                vscode.postMessage({ type: 'refresh' });
            });
        }
        
        // Add event listener for query execution
        const queryButton = document.getElementById('execute-query-btn');
        if (queryButton) {
            queryButton.addEventListener('click', function() {
                const queryInput = document.getElementById('query-input');
                if (queryInput && queryInput.value.trim()) {
                    vscode.postMessage({ 
                        type: 'executeQuery', 
                        query: queryInput.value.trim() 
                    });
                }
            });
        }
        
        // Add enter key support for query input
        const queryInput = document.getElementById('query-input');
        if (queryInput) {
            queryInput.addEventListener('keypress', function(e) {
                if (e.key === 'Enter' && !e.shiftKey) {
                    e.preventDefault();
                    const query = this.value.trim();
                    if (query) {
                        vscode.postMessage({ 
                            type: 'executeQuery', 
                            query: query 
                        });
                    }
                }
            });
        }
    }
    
    function requestInitialData() {
        vscode.postMessage({ type: 'ready' });
    }
    
    function updateStatus(data) {
        if (statusElement) {
            statusElement.textContent = data.message || 'Unknown status';
            statusElement.className = `status ${data.type || 'info'}`;
        }
    }
    
    function updateBackendStatus(status) {
        if (backendStatusElement) {
            const isRunning = status.isRunning;
            backendStatusElement.innerHTML = `
                <span class="status-indicator ${isRunning ? 'running' : 'stopped'}"></span>
                Backend: ${isRunning ? 'Running' : 'Stopped'}
                ${status.port ? ` (Port: ${status.port})` : ''}
            `;
        }
    }
    
    function updateInstallationStatus(status) {
        if (installationStatusElement) {
            const isInstalled = status.isInstalled;
            installationStatusElement.innerHTML = `
                <span class="status-indicator ${isInstalled ? 'installed' : 'not-installed'}"></span>
                SWI-Prolog: ${isInstalled ? 'Installed' : 'Not Found'}
                ${status.version ? ` (${status.version})` : ''}
                ${status.path ? `<br><small>Path: ${status.path}</small>` : ''}
            `;
        }
    }
    
    function showError(error) {
        if (errorContainer) {
            errorContainer.style.display = 'block';
            errorContainer.innerHTML = `
                <div class="error-message">
                    <strong>Error:</strong> ${error.message || error}
                    ${error.details ? `<br><small>${error.details}</small>` : ''}
                </div>
            `;
        }
    }
    
    function clearError() {
        if (errorContainer) {
            errorContainer.style.display = 'none';
            errorContainer.innerHTML = '';
        }
    }
    
    function displayQueryResult(result) {
        const resultContainer = document.getElementById('query-result');
        if (resultContainer) {
            if (result.success) {
                resultContainer.innerHTML = `
                    <div class="query-success">
                        <h4>Query Result:</h4>
                        <pre>${JSON.stringify(result.data, null, 2)}</pre>
                    </div>
                `;
            } else {
                resultContainer.innerHTML = `
                    <div class="query-error">
                        <h4>Query Failed:</h4>
                        <pre>${result.error || 'Unknown error'}</pre>
                    </div>
                `;
            }
        }
    }
    
    // Handle messages from the extension
    window.addEventListener('message', event => {
        const message = event.data;
        
        switch (message.type) {
            case 'statusUpdate':
                clearError();
                if (message.backendStatus) {
                    updateBackendStatus(message.backendStatus);
                }
                if (message.installationStatus) {
                    updateInstallationStatus(message.installationStatus);
                }
                if (message.status) {
                    updateStatus(message.status);
                }
                break;
                
            case 'error':
                showError(message.error);
                break;
                
            case 'queryResult':
                displayQueryResult(message.result);
                break;
                
            case 'refresh':
                // Clear previous results
                clearError();
                const queryResult = document.getElementById('query-result');
                if (queryResult) {
                    queryResult.innerHTML = '';
                }
                break;
        }
    });
    
    // Utility functions for theme support
    function getThemeColor(property) {
        return getComputedStyle(document.documentElement).getPropertyValue(property);
    }
    
    // Auto-refresh status every 30 seconds
    setInterval(function() {
        vscode.postMessage({ type: 'refresh' });
    }, 30000);
})();