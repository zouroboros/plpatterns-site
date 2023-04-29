import hl from "highlight.js"

export function highlight(language, code) {
    const lines = code.split("\n")
    const html = lines
        .map(line => hl.highlight(line, { language }).value)
        .map(line => `<code>${line}</code>`)
        .join("\n")
    return html
}