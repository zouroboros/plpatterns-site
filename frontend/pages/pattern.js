import * as api from "../api.js"
import { highlight } from "../highlight.js"
import * as cmark from "commonmark"

export default async function ( { languageName, alias }, container) {
    const decodedAlias = decodeURI(alias)
    const pattern = await api.loadPatternByLanguageAndName(languageName, decodedAlias)
    container.innerHTML = `<h1>${pattern.name}</h1>`
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