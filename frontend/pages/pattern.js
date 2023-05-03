import * as api from "../api.js"
import { highlight } from "../highlight.js"
import * as cmark from "commonmark"

export default async function ( { languageName, patternName }, container) {
    const decodedPatterName = decodeURI(patternName)
    container.innerHTML = `<h1>${decodedPatterName}</h1>`
    const pattern = await api.loadPatternByLanguageAndName(languageName, decodedPatterName)
    const highlightedCode = await highlight(languageName, pattern.code)
    const reader = new cmark.Parser();
    const writer = new cmark.HtmlRenderer();
    const descriptionHtml = writer.render(reader.parse(pattern.description))
    container.innerHTML += `<div class="snippet">
            <pre>${highlightedCode}</pre>
        </div>
        <div>
            <p>${descriptionHtml}</p>
        </div>`
}