import { expect } from 'chai';
import { CancellationToken, ChatRequest, ChatResponseStream } from 'vscode';
import { ChatCommandRegistry } from '../src/modules/chatCommandRegistry';

describe('Agent Context Awareness Chat Commands', function () {
  let registry: ChatCommandRegistry;
  let mockStream: any;
  let lastMarkdown: string[];

  beforeEach(() => {
    registry = new ChatCommandRegistry();
    lastMarkdown = [];
    mockStream = {
      markdown: (msg: string) => lastMarkdown.push(msg)
    };
  });

  it('should return environment context for /env', async function () {
    const handler = registry.findHandler('env');
    expect(handler).to.exist;
    await handler!.handle('', {} as ChatRequest, mockStream as ChatResponseStream, {} as CancellationToken, { prologBackend: null, telemetry: null });
    expect(lastMarkdown[0]).to.include('Environment Context');
    expect(lastMarkdown[1]).to.include('workspaceFolders');
  });

  it('should list files for /list_files', async function () {
    const handler = registry.findHandler('list_files');
    expect(handler).to.exist;
    await handler!.handle('', {} as ChatRequest, mockStream as ChatResponseStream, {} as CancellationToken, { prologBackend: null, telemetry: null });
    expect(lastMarkdown[0]).to.include('Workspace Files');
    expect(lastMarkdown[1]).to.include('[');
  });

  it('should return active file info for /active_file', async function () {
    const handler = registry.findHandler('active_file');
    expect(handler).to.exist;
    await handler!.handle('', {} as ChatRequest, mockStream as ChatResponseStream, {} as CancellationToken, { prologBackend: null, telemetry: null });
    expect(lastMarkdown[0]).to.match(/Active File Info|No active file/);
  });
});
