import * as fs from 'fs';
import fg from 'fast-glob';
interface ISnippet {
  prefix: string;
  body: string;
  description: string;
}
interface ISnippets {
  [key: string]: ISnippet;
}
const snippets: ISnippets = {};
async function libsToSnippets(path: string, builtin: boolean) {
  // Use fast-glob to find all directories in the given path
  const libs = await fg([`${path}/*/`], { onlyDirectories: true });
  await Promise.all(
    libs.map(async lib => {
      // Use fast-glob to find all .txt files in the library directory
      const preds = await fg([`${lib}/*.txt`], { onlyFiles: true });
      await Promise.all(
        preds.map(async (pred: string) => {
          const snippet = await fileToSnippet(pred);
          if (snippet) {
            let key = pred
              .split('/')
              .slice(-2)
              .join(':')
              .replace('-', '/')
              .replace(/\.txt$/, '');
            if (builtin) {
              key = key.split(':')[1];
            }
            snippets[key] = snippet;
          }
        })
      );
    })
  );
}
async function fileToSnippet(file: string) {
  if (/summary\.txt$/.test(file)) {
    return null;
  }
  try {
    let snippet: ISnippet | null = null;
    const txt = await fs.promises.readFile(file, 'utf8');
    const str = txt.toString().replace(/\n\n+/g, '\n\n').replace(/ {8,}/g, '    ').trim();
    const match = str.match(/^(\w+)(\(([^)]*)\))?/);
    let prefix: string = '',
      params: string = '',
      body: string;
    if (match) {
      prefix = match[1];
      body = prefix;
      if (match[2]) {
        params = match[3];
        const plist: string[] = params.split(',');
        body += '(';
        for (let i = 1; i <= plist.length; i++) {
          const mtch = plist[i - 1].match(/\w+/);
          if (mtch) {
            const pName = mtch[0];
            body += '${' + i + ':' + pName + '}';
            if (i < plist.length) {
              body += ', ';
            } else {
              body += ')$' + (i + 1) + '\n$0';
            }
          }
        }
      }

      snippet = {
        prefix: prefix,
        body: body,
        description: str,
      };
    }
    return snippet;
  } catch (error) {
    console.log(error);
    return undefined;
  }
}

(async () => {
  const docRoot = '/opt/eclipseclp/doc/bips/';
  await libsToSnippets(docRoot + 'kernel', true);
  await libsToSnippets(docRoot + 'lib', false);
  await libsToSnippets(docRoot + 'lib_public', false);
  await fs.promises.writeFile('prolog.ecl.json', JSON.stringify(snippets, null, 2));
})();
