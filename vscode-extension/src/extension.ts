// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';
import * as O from 'fp-ts/Option';
import { pipe } from 'fp-ts/function';
import * as fs from 'fs';
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

	function nodePath(nid: String) {
		return banyanDir + "/" + nid + ".md";
	}

	function editNode(nid: String) {
		let path = nodePath(nid);
		vscode.window.showInformationMessage("Opening " + path);
		vscode.commands.executeCommand('vscode.open', vscode.Uri.file(path));
	}

	function newNode(nid: String, contents: string) {
		vscode.window.showInformationMessage(contents);
		let path = nodePath(nid);
		fs.stat(path, (err, _stats) => {
			if (err !== null) {
				vscode.window.showErrorMessage("File already exists: " + path);
			} else {
				fs.writeFile(path, contents, (err) => {
					if (err) {
						vscode.window.showErrorMessage(err.toString());
					} else {
						editNode(nid);
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
			// Add your code for what to do when the authentication completes here.
			vscode.window.showInformationMessage(uri.toString());
			let [_scheme, action, nid] = uri.path.split("/");
			if (action === "new") {
				let queries = uri.query.split('&');
				if (queries.length > 0) {
					let [k, v] = queries[0].split('=');
					let mParent = (k === "parent") ? O.some(v) : O.none;
					newNode(nid, mdTemplate(mParent));
				};
			} else if (action === "edit") {
				editNode(nid);
			};
		}
	}));

}

// this method is called when your extension is deactivated
export function deactivate() { }
