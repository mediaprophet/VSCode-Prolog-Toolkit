import * as fs from 'fs';
import { CancellationToken, ChatRequest, ChatResponseStream } from 'vscode';
import { ChatCommand, ChatCommandContext } from './types';

function toRdfJson(triples: Array<{ subject: string, predicate: string, object: string }>): any {
  // Convert triples to RDF/JSON (https://www.w3.org/TR/rdf-json/)
  const rdf: any = {};
  for (const { subject, predicate, object } of triples) {
    if (!rdf[subject]) rdf[subject] = {};
    if (!rdf[subject][predicate]) rdf[subject][predicate] = [];
    rdf[subject][predicate].push({ value: object, type: 'literal' });
  }
  return rdf;
}

export class DescribeCommand implements ChatCommand {
  name = '/describe';
  description = 'Describe a Prolog or N3 file or snippet, extracting predicates.';
  arguments = [
    {
      name: 'fileOrSnippet',
      type: 'string',
      description: 'The file path or code snippet to describe.',
      example: 'family.pl',
      required: true
    },
    {
      name: 'format',
      type: 'string',
      description: 'Output format: markdown, json, or rdf-json (default: markdown).',
      example: 'json',
      required: false
    }
  ];
  canHandle(command: string): boolean {
    return command === 'describe' || command === '/describe';
  }

  async handle(args: string, request: ChatRequest, stream: ChatResponseStream, token: CancellationToken, context: ChatCommandContext): Promise<void> {
    // Usage: /describe <file> [format=markdown|json|rdf-json]
    const parts = args.trim().split(/\s+/);
    const fileOrSnippet = parts[0];
    const format = (parts[1] || 'markdown').toLowerCase();
    if (!fileOrSnippet) {
      stream.markdown('Usage: /describe <file> [format=markdown|json|rdf-json]');
      return;
    }
    let code = '';
    let filePath = '';
    if (fs.existsSync(fileOrSnippet)) {
      filePath = fileOrSnippet;
      code = fs.readFileSync(fileOrSnippet, 'utf8');
    } else {
      code = fileOrSnippet;
    }
    // Simple Prolog/N3 predicate extraction (stub)
    const predicates = Array.from(code.matchAll(/([a-zA-Z0-9_]+)\s*\(/g)).map(m => m[1]);
    const summary = `**Predicates found:** ${predicates.join(', ')}`;
    const triples = predicates.map(p => ({ subject: filePath || 'snippet', predicate: 'hasPredicate', object: String(p) }));
    if (format === 'markdown') {
      stream.markdown(summary);
    } else if (format === 'json') {
      stream.markdown('```json\n' + JSON.stringify({ predicates }, null, 2) + '\n```');
    } else if (format === 'rdf-json') {
      stream.markdown('```json\n' + JSON.stringify(toRdfJson(triples), null, 2) + '\n```');
    } else {
      stream.markdown('Unknown format. Use markdown, json, or rdf-json.');
    }
  }
}
