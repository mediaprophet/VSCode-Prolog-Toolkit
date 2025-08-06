# Prolog LSP Setup Script for Windows

Write-Host "Setting up Prolog LSP for multiple IDEs..." -ForegroundColor Green

# Check if Node.js is installed
try {
    $nodeVersion = node --version
    Write-Host "Node.js found: $nodeVersion" -ForegroundColor Green
} catch {
    Write-Host "Error: Node.js is required but not installed." -ForegroundColor Red
    Write-Host "Please install Node.js from https://nodejs.org/" -ForegroundColor Yellow
    exit 1
}

# Check if SWI-Prolog is installed
try {
    $swiplVersion = swipl --version
    Write-Host "SWI-Prolog found: $swiplVersion" -ForegroundColor Green
} catch {
    Write-Host "Warning: SWI-Prolog not found in PATH." -ForegroundColor Yellow
    Write-Host "Please install SWI-Prolog from https://www.swi-prolog.org/" -ForegroundColor Yellow
}

Write-Host "Available IDE configurations:" -ForegroundColor Cyan
Get-ChildItem -Filter "*.json" | ForEach-Object {
    $ideName = $_.BaseName
    Write-Host "  - $ideName" -ForegroundColor White
}

Write-Host ""
Write-Host "To use with your IDE:" -ForegroundColor Cyan
Write-Host "1. Copy the appropriate configuration file" -ForegroundColor White
Write-Host "2. Follow the setup instructions in the documentation" -ForegroundColor White
Write-Host "3. Restart your IDE" -ForegroundColor White

Write-Host "Setup complete!" -ForegroundColor Green
