#!/usr/bin/env node

const fs = require('fs');
const path = require('path');

/**
 * Version bump script for New-VSC-Prolog extension
 * Usage: node scripts/version-bump.js [major|minor|patch]
 */

function updatePackageVersion(versionType) {
    const packagePath = path.join(__dirname, '..', 'package.json');
    const packageJson = JSON.parse(fs.readFileSync(packagePath, 'utf8'));
    
    const currentVersion = packageJson.version;
    const versionParts = currentVersion.split('.').map(Number);
    
    switch (versionType) {
        case 'major':
            versionParts[0]++;
            versionParts[1] = 0;
            versionParts[2] = 0;
            break;
        case 'minor':
            versionParts[1]++;
            versionParts[2] = 0;
            break;
        case 'patch':
        default:
            versionParts[2]++;
            break;
    }
    
    const newVersion = versionParts.join('.');
    packageJson.version = newVersion;
    
    fs.writeFileSync(packagePath, JSON.stringify(packageJson, null, 4));
    
    console.log(`Version bumped from ${currentVersion} to ${newVersion}`);
    return newVersion;
}

function updateChangelog(newVersion, versionType) {
    const changelogPath = path.join(__dirname, '..', 'CHANGELOG.md');
    const changelog = fs.readFileSync(changelogPath, 'utf8');
    
    const today = new Date().toISOString().split('T')[0];
    const releaseHeader = `## [${newVersion}] - ${today}`;
    
    // Replace [Unreleased] with the new version
    const updatedChangelog = changelog.replace(
        '## [Unreleased]',
        `## [Unreleased]\n\n### Added\n### Changed\n### Fixed\n### Removed\n\n${releaseHeader}`
    );
    
    fs.writeFileSync(changelogPath, updatedChangelog);
    console.log(`Updated CHANGELOG.md with version ${newVersion}`);
}

function createGitTag(version) {
    const { execSync } = require('child_process');
    
    try {
        // Add and commit changes
        execSync('git add package.json CHANGELOG.md', { stdio: 'inherit' });
        execSync(`git commit -m "chore: bump version to ${version}"`, { stdio: 'inherit' });
        
        // Create and push tag
        execSync(`git tag -a v${version} -m "Release version ${version}"`, { stdio: 'inherit' });
        console.log(`Created git tag v${version}`);
        
        console.log('To push the tag, run: git push origin --tags');
    } catch (error) {
        console.warn('Git operations failed. Please commit and tag manually.');
        console.warn('Commands to run:');
        console.warn(`  git add package.json CHANGELOG.md`);
        console.warn(`  git commit -m "chore: bump version to ${version}"`);
        console.warn(`  git tag -a v${version} -m "Release version ${version}"`);
        console.warn(`  git push origin --tags`);
    }
}

function validateVersion(versionType) {
    const validTypes = ['major', 'minor', 'patch'];
    if (!validTypes.includes(versionType)) {
        console.error(`Invalid version type: ${versionType}`);
        console.error(`Valid types: ${validTypes.join(', ')}`);
        process.exit(1);
    }
}

function main() {
    const versionType = process.argv[2] || 'patch';
    
    validateVersion(versionType);
    
    console.log(`Bumping ${versionType} version...`);
    
    const newVersion = updatePackageVersion(versionType);
    updateChangelog(newVersion, versionType);
    
    // Ask if user wants to create git tag
    const readline = require('readline');
    const rl = readline.createInterface({
        input: process.stdin,
        output: process.stdout
    });
    
    rl.question('Create git tag and commit? (y/N): ', (answer) => {
        if (answer.toLowerCase() === 'y' || answer.toLowerCase() === 'yes') {
            createGitTag(newVersion);
        } else {
            console.log('Skipped git operations. Remember to commit and tag manually.');
        }
        rl.close();
    });
}

if (require.main === module) {
    main();
}

module.exports = {
    updatePackageVersion,
    updateChangelog,
    createGitTag
};