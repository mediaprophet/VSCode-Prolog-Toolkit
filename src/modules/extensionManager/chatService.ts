import { PrologBackend } from '../../prologBackend/PrologBackend';
import { ChatHandler } from '../chatHandler';

export class ChatService {
  private chatHandler: ChatHandler;
  constructor(chatHandler: ChatHandler) {
    this.chatHandler = chatHandler;
  }

  handleChatRequest(request: any, context: any, stream: any, token: any) {
    return this.chatHandler.handleChatRequest(request, context, stream, token);
  }

  updateBackend(backend: PrologBackend) {
    this.chatHandler.updateBackend(backend);
  }
}
