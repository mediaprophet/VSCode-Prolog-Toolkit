import { PrologRefactor } from "./prologRefactor";
import { Utils } from "../utils/utils";
import {
  ReferenceProvider,
  TextDocument,
  Position,
  ReferenceContext,
  CancellationToken,
  Location,
  window,
  workspace,
  Uri
} from "vscode";
import * as fs from "fs";
import { spawn } from "process-promises";
export class PrologReferenceProvider implements ReferenceProvider {
  constructor() {}
  // Implement the provideReferences method required by ReferenceProvider interface
  public provideReferences(
    doc: TextDocument,
    position: Position,
    context: ReferenceContext,
    token: CancellationToken
  ): Location[] {
    let docContent = doc.getText(); // Get the content of the entire document as a string
    // Define a regular expression for finding occurrences of the predicate in the document
    let pred = Utils.getPredicateUnderCursor(doc, position);
    if (!pred) {
      return [];
    }
    var regex= "\\((.|\\s)*?\\)"
    const regexp = new RegExp(pred.functor+regex,"gm");
    const regexpModule = /^\s*:-\s*use_module\(([a-zA-Z0-9_\/]*|(\"|\')[a-zA-Z0-9_\/\.]*(\"|\'))\s*((,\s*[\/a-zA-Z0-9\[\]]*\s*\)|\))\s*\.)/gm;// Define a regular expression for finding "use_module" declarations in the document
    const arrayModule = [...docContent.matchAll(regexpModule)]// Extract "use_module" declarations from the document
    const filenameParts = doc.fileName.split(".");
    const prolog = filenameParts.length > 1 ? filenameParts[1] : 'pl';// Extract the Prolog dialect from the file extension
    const array = [...docContent.matchAll(regexp)]; // Extract occurrences of the predicate in the document
    var locations = array.map((elem)=> elem.index !== undefined ? new Location(Uri.file(doc.fileName),doc.positionAt(elem.index)) : null).filter(loc => loc !== null) as Location[];// Create an array to store Location objects
    // Iterate through "use_module" declarations
    for(let i = 0 ; i < arrayModule.length;i++){
      if (arrayModule[i] && arrayModule[i][1]) {
        var modpath = arrayModule[i][1].replace(new RegExp("\\'","gm"),"")
        modpath = modpath.replace(new RegExp('\\"',"gm"),"")
        var text ="";
        try {
          if (workspace.workspaceFolders && workspace.workspaceFolders[0]) {
            text=fs.readFileSync(workspace.workspaceFolders[0].uri.fsPath+"\/"+modpath+"."+prolog, 'utf8');// Read the content of the referenced module file
          }
        } catch (error) {
           console.error("Error reading file:", error);
        }
        const array = [...text.matchAll(regexp)];// Extract occurrences of the predicate in the referenced module file
        if (workspace.workspaceFolders && workspace.workspaceFolders[0]) {
          const newLocations = array.map((elem)=> elem.index !== undefined ? new Location(Uri.file(workspace.workspaceFolders[0].uri.fsPath+"/"+modpath+"."+prolog),Utils.findLineColForByte(text,elem.index)) : null).filter(loc => loc !== null) as Location[];
          locations = locations.concat(newLocations);// Append the new occurrences to the locations array
        }
      }
    }
    // Return the array of Location objects
    return locations
  }
}
