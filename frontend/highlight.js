import hljs from 'highlight.js/lib/core';

export async function highlight(language, code) {
    const lines = code.split("\n")
    const lowercaseLanguageName = language.toLowerCase()
    const languageModule = await import(`./node_modules/highlight.js/es/languages/${lowercaseLanguageName}.js`)
    hljs.registerLanguage(lowercaseLanguageName, languageModule.default)
    const html = lines
        .map(line => hljs.highlight(line, { language }).value)
        .map(line => `<code>${line}</code>`)
        .join("\n")
    return html
}