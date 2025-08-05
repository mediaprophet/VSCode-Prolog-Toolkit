#!/usr/bin/env node

const fs = require('fs');
const path = require('path');

/**
 * Setup test environment for the VS Code Prolog extension
 * This script ensures all necessary directories and files exist for testing
 */

function ensureDir(dirPath) {
  if (!fs.existsSync(dirPath)) {
    fs.mkdirSync(dirPath, { recursive: true });
    console.log(`Created directory: ${dirPath}`);
  }
}

function ensureFile(filePath, content) {
  if (!fs.existsSync(filePath)) {
    fs.writeFileSync(filePath, content);
    console.log(`Created file: ${filePath}`);
  }
}

function cleanupOldLogs() {
  const logsDir = 'test/logs';
  const oldLogsDir = 'test/logs/oldLogs';
  
  if (!fs.existsSync(logsDir)) return;
  
  const now = Date.now();
  const maxAge = 7 * 24 * 60 * 60 * 1000; // 7 days in milliseconds
  
  try {
    const files = fs.readdirSync(logsDir);
    let movedCount = 0;
    
    files.forEach(file => {
      const filePath = path.join(logsDir, file);
      const stat = fs.statSync(filePath);
      
      if (stat.isFile() && (now - stat.mtime.getTime()) > maxAge) {
        if (!fs.existsSync(oldLogsDir)) {
          fs.mkdirSync(oldLogsDir, { recursive: true });
        }
        
        const oldPath = path.join(oldLogsDir, file);
        fs.renameSync(filePath, oldPath);
        movedCount++;
      }
    });
    
    if (movedCount > 0) {
      console.log(`Moved ${movedCount} old log files to ${oldLogsDir}`);
    }
    
    // Clean up very old files in oldLogs (older than 30 days)
    if (fs.existsSync(oldLogsDir)) {
      const veryOldMaxAge = 30 * 24 * 60 * 60 * 1000; // 30 days
      const oldFiles = fs.readdirSync(oldLogsDir);
      let deletedCount = 0;
      
      oldFiles.forEach(file => {
        const filePath = path.join(oldLogsDir, file);
        const stat = fs.statSync(filePath);
        
        if (stat.isFile() && (now - stat.mtime.getTime()) > veryOldMaxAge) {
          fs.unlinkSync(filePath);
          deletedCount++;
        }
      });
      
      if (deletedCount > 0) {
        console.log(`Deleted ${deletedCount} very old log files`);
      }
    }
  } catch (error) {
    console.warn('Warning: Could not clean up old logs:', error.message);
  }
}

function createTestLogConfig() {
  const logConfig = {
    "logLevel": "info",
    "logToFile": true,
    "logToConsole": true,
    "maxLogFiles": 10,
    "maxLogSize": "10MB",
    "logRotation": true,
    "testLogRetention": "7d",
    "archiveOldLogs": true
  };
  
  const configPath = path.join('test', 'log-config.json');
  ensureFile(configPath, JSON.stringify(logConfig, null, 2));
}

function main() {
  console.log('Setting up test environment...');

  // Clean up old logs first
  cleanupOldLogs();

  // Ensure test directories exist
  const testDirs = [
    'test/logs',
    'test/logs/oldLogs',
    'test/resources',
    'test/temp',
    'test/coverage',
    'logs'
  ];

  testDirs.forEach(ensureDir);

  // Ensure test resource files exist
  const testResourcesDir = 'test/resources';
  
  // Create a sample Prolog file with PlDoc documentation
  const samplePrologFile = path.join(testResourcesDir, 'foo_with_pldoc.pl');
  const samplePrologContent = `%! foo(+X:integer, -Y:integer) is det.
%
%  A simple predicate that adds 1 to X to get Y.
%  This is used for testing PlDoc integration.
%
%  @param X The input integer
%  @param Y The output integer (X + 1)
%  @example
%    ?- foo(5, Y).
%    Y = 6.
%
foo(X, Y) :-
    Y is X + 1.

%! bar(+List:list, -Length:integer) is det.
%
%  Calculate the length of a list.
%
%  @param List Input list
%  @param Length The length of the list
%
bar(List, Length) :-
    length(List, Length).
`;

  ensureFile(samplePrologFile, samplePrologContent);

  // Create a sample N3 file for N3Logic testing
  const sampleN3File = path.join(testResourcesDir, 'sample.n3');
  const sampleN3Content = `@prefix : <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

:socrates a :Person .
:Person rdfs:subClassOf :Mortal .

{ ?x a :Person } => { ?x a :Mortal } .
`;

  ensureFile(sampleN3File, sampleN3Content);

  // Create test configuration
  const testConfigFile = path.join('test', 'test-config.json');
  const testConfig = {
    "timeout": 10000,
    "retries": 2,
    "slow": 2000,
    "testResourcesPath": "./test/resources",
    "logPath": "./test/logs"
  };

  ensureFile(testConfigFile, JSON.stringify(testConfig, null, 2));

  // Create log configuration
  createTestLogConfig();

  // Create additional test resources
  createAdditionalTestResources();

  console.log('Test environment setup complete!');
}

function createAdditionalTestResources() {
  const resourcesDir = 'test/resources';
  
  // Create a complex N3 file for advanced testing
  const complexN3File = path.join(resourcesDir, 'complex.n3');
  const complexN3Content = `@prefix : <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

# Basic facts
:socrates a :Person ;
    :hasName "Socrates" ;
    :bornIn :Athens ;
    :hasTeacher :nobody .

:plato a :Person ;
    :hasName "Plato" ;
    :bornIn :Athens ;
    :hasTeacher :socrates .

:aristotle a :Person ;
    :hasName "Aristotle" ;
    :bornIn :Stagira ;
    :hasTeacher :plato .

# Class hierarchy
:Person rdfs:subClassOf :LivingBeing .
:LivingBeing rdfs:subClassOf :Entity .
:Philosopher rdfs:subClassOf :Person .

# Rules for reasoning
{ ?x a :Person } => { ?x a :Mortal } .
{ ?x :hasTeacher ?y } => { ?y a :Teacher } .
{ ?x a :Teacher ; a :Person } => { ?x a :Philosopher } .

# Complex reasoning rule
{ ?x :bornIn ?place . ?y :bornIn ?place . ?x owl:differentFrom ?y } => { ?x :compatriot ?y } .
`;

  ensureFile(complexN3File, complexN3Content);

  // Create a test Prolog file with various predicates
  const testPrologFile = path.join(resourcesDir, 'test_predicates.pl');
  const testPrologContent = `%! factorial(+N:integer, -F:integer) is det.
%
%  Calculate factorial of N.
%
factorial(0, 1) :- !.
factorial(N, F) :-
    N > 0,
    N1 is N - 1,
    factorial(N1, F1),
    F is N * F1.

%! fibonacci(+N:integer, -F:integer) is det.
%
%  Calculate Nth Fibonacci number.
%
fibonacci(0, 0) :- !.
fibonacci(1, 1) :- !.
fibonacci(N, F) :-
    N > 1,
    N1 is N - 1,
    N2 is N - 2,
    fibonacci(N1, F1),
    fibonacci(N2, F2),
    F is F1 + F2.

%! list_length(+List:list, -Length:integer) is det.
%
%  Calculate length of a list.
%
list_length([], 0).
list_length([_|T], N) :-
    list_length(T, N1),
    N is N1 + 1.

%! append_lists(+List1:list, +List2:list, -Result:list) is det.
%
%  Append two lists.
%
append_lists([], L, L).
append_lists([H|T1], L2, [H|T3]) :-
    append_lists(T1, L2, T3).
`;

  ensureFile(testPrologFile, testPrologContent);

  // Create error test cases
  const errorTestFile = path.join(resourcesDir, 'error_cases.pl');
  const errorTestContent = `% This file contains intentional errors for testing error handling

% Syntax error case
syntax_error_predicate(X :-
    write(X).

% Undefined predicate call
test_undefined :-
    undefined_predicate(test).

% Type error case
type_error_test :-
    X is atom + 5.
`;

  ensureFile(errorTestFile, errorTestContent);
}

if (require.main === module) {
  main();
}

module.exports = { main };