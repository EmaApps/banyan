// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';
import * as O from 'fp-ts/Option';
let fs = require("fs");
var dedent = require('dedent-js');

// Hardcoding for now.
let banyanDir = "/Users/srid/Banyan/content";

// this method is called when your extension is activated
// your extension is activated the very first time the command is executed
export function activate(context: vscode.ExtensionContext) {

	// Use the console to output diagnostic information (console.log) and errors (console.error)
	// This line of code will only be executed once when your extension is activated
	console.log('Congratulations, your extension "banyan" is now active!');

	// The command has been defined in the package.json file
	// Now provide the implementation of the command with registerCommand
	// The commandId parameter must match the command field in package.json
	context.subscriptions.push(vscode.commands.registerCommand('banyan.helloWorld', () => {
		// The code you place here will be executed every time your command is executed
		// Display a message box to the user
		vscode.window.showInformationMessage('Hello Srid!');
	}));

	function newNode(fp: String, contents: string) {
		// TODO: actually create the file; and open in VSCode
		vscode.window.showInformationMessage(contents);
		let path = banyanDir + "/" + fp + ".md";
		fs.writeFile(path, contents, (err : string)  => {
			vscode.window.showErrorMessage(err);
		});
	};

	context.subscriptions.push(vscode.window.registerUriHandler({
		handleUri(uri: vscode.Uri): vscode.ProviderResult<void> {
			// Add your code for what to do when the authentication completes here.
			vscode.window.showInformationMessage(uri.toString());
			let [_empty, _new, fp] = uri.path.split("/");
			let dt = new Date().toISOString();
			let queries = uri.query.split('&');
			if (queries.length > 0) {
				let [k, v] = queries[0].split('=');
				// TODO: handle non-parent case
				if (k === "parent") { 
					let mParent = O.some(v);
					newNode(fp, dedent(`---
						date: ${dt}
						parent: ${v}
						---
            `));
				};
			};
		}
	}));

}

// this method is called when your extension is deactivated
export function deactivate() { }
