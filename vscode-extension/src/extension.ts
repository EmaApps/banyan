// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';
import * as O from 'fp-ts/Option';
import { pipe } from 'fp-ts/function';
import * as fs from 'fs';
import * as url from 'url';
var dedent = require('dedent-js');

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

	function nodePath(baseDir: string, nid: String) {
		return baseDir + "/" + nid + ".md";
	}

	function editNode(baseDir: string, nid: String) {
		let path = nodePath(baseDir, nid);
		vscode.window.showInformationMessage("Opening " + path);
		vscode.commands.executeCommand('vscode.open', vscode.Uri.file(path));
	}

	function newNode(baseDir: string, nid: String, contents: string) {
		vscode.window.showInformationMessage(contents);
		let path = nodePath(baseDir, nid);
		fs.stat(path, (err, stats) => {
			if (err === null) {
				vscode.window.showErrorMessage("File already exists: " + path);
				vscode.window.showInformationMessage(stats.toString());
			} else {
				fs.writeFile(path, contents, (err) => {
					if (err) {
						vscode.window.showErrorMessage(err.toString());
					} else {
						editNode(baseDir, nid);
					}
				});
			};
		});
	};

	function mdTemplate(mParent: O.Option<string>) {
		let dt = new Date().toISOString();
		let parentS = pipe(
			mParent,
			O.match(() => ``, parent => `parent: ${parent}\n`)
		);
		return dedent(`---
						date: ${dt}
						${parentS}---


            `);
	};

	context.subscriptions.push(vscode.window.registerUriHandler({
		handleUri(uri: vscode.Uri): vscode.ProviderResult<void> {
			let [_scheme, action, nid] = uri.path.split("/");
			let queries = new url.URLSearchParams(uri.query);
			let baseDir = queries.get("baseDir");
			if (baseDir === null) {
				vscode.window.showErrorMessage("baseDir is required");
			} else {
				if (action === "new") {
					let mParent = O.fromNullable(queries.get("parent"));
					newNode(baseDir, nid, mdTemplate(mParent));
				} else if (action === "edit") {
					editNode(baseDir, nid);
				};
			};
		}
	}));

}

// this method is called when your extension is deactivated
export function deactivate() { }
