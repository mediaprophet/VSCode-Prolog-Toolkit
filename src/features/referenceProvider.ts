import * as fs from 'fs';
import {
  CancellationToken,
  Location,
  Position,
  ReferenceContext,
  ReferenceProvider,
  TextDocument,
  Uri,
  workspace,
} from 'vscode';
import { PositionUtils, SnippetUtils } from '../utils/utils';
export class PrologReferenceProvider implements ReferenceProvider {
  constructor() {
    // No initialization required for reference provider
  }
  // Implement the provideReferences method required by ReferenceProvider interface
  public provideReferences(
    doc: TextDocument,
    position: Position,
    _context: ReferenceContext,
    _token: CancellationToken
  ): Location[] {
    const docContent = doc.getText(); // Get the content of the entire document as a string
    // Define a regular expression for finding occurrences of the predicate in the document
    const pred = SnippetUtils.getPredicateUnderCursor(doc, position);
    if (!pred) {
      return [];
    }
    const regex = '\\((.|\\s)*?\\)';
    const regexp = new RegExp(pred.functor + regex, 'gm');
    const regexpModule =
      /^\s*:-\s*use_module\(([a-zA-Z0-9_/]*|("|')[a-zA-Z0-9_/.]*("|'))\s*((,\s*[/a-zA-Z0-9[\]]*\s*\)|\))\s*\.)/gm; // Define a regular expression for finding "use_module" declarations in the document
    const arrayModule = [...docContent.matchAll(regexpModule)]; // Extract "use_module" declarations from the document
    const filenameParts = doc.fileName.split('.');
    const prolog = filenameParts.length > 1 ? filenameParts[1] : 'pl'; // Extract the Prolog dialect from the file extension
    const array = [...docContent.matchAll(regexp)]; // Extract occurrences of the predicate in the document
    let locations = array
      .map(elem =>
        elem.index !== undefined
          ? new Location(Uri.file(doc.fileName), doc.positionAt(elem.index))
          : null
      )
      .filter(loc => loc !== null); // Create an array to store Location objects
    // Iterate through "use_module" declarations
    for (let i = 0; i < arrayModule.length; i++) {
      const modpathRaw = arrayModule?.[i]?.[1] ?? '';
      if (!modpathRaw) {
        continue;
      }
      let modpath = modpathRaw.replace(new RegExp("'", 'gm'), '');
      if (!modpath) {
        continue;
      }
      modpath = modpath.replace(new RegExp('"', 'gm'), '');
      let text = '';
      try {
        const wsRoot = workspace.workspaceFolders?.[0]?.uri?.fsPath;
        if (!wsRoot) {
          continue;
        }
        text = fs.readFileSync(wsRoot + '/' + modpath + '.' + prolog, 'utf8');
      } catch (error) {
        console.error('Error reading file:', error);
        continue;
      }
      const array = [...text.matchAll(regexp)]; // Extract occurrences of the predicate in the referenced module file
      const wsRoot = workspace.workspaceFolders?.[0]?.uri?.fsPath;
      if (!wsRoot) {
        continue;
      }
      const newLocations = array
        .map(elem =>
          elem.index !== undefined
            ? new Location(
              Uri.file(wsRoot + '/' + modpath + '.' + prolog),
              PositionUtils.findLineColForByte(text, elem.index)
            )
            : null
        )
        .filter(loc => loc !== null);
      locations = locations.concat(newLocations); // Append the new occurrences to the locations array
    }
    // Return the array of Location objects
    return locations;
  }
}
