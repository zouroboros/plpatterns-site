import * as api from "../api.js"
import { highlight } from "../highlight.js"
import * as cmark from "commonmark"

export default async function ( { languageName, alias }, container) {
    const decodedAlias = decodeURI(alias)
    const pattern = await api.loadPatternByLanguageAndAlias(languageName, decodedAlias)
    container.innerHTML = `<h1>${pattern.name}</h1>`
    const patternHtml = await Promise.all(pattern.parts.map(part => renderPart(languageName, part)))
    container.insertAdjacentHTML("beforeend", patternHtml.join(""))
}

async function renderPart(languageName, part) {
    if (part.partType === "Markup") {
        const reader = new cmark.Parser();
        const writer = new cmark.HtmlRenderer();
        const partHtml = writer.render(reader.parse(part.text))
        return `<div>
            <p>${partHtml}</p>
        </div>`
    } else if (part.partType === "Code") {
        const highlightedCode = await highlight(languageName, part.text)
        return `<div class="snippet">
            <pre>${highlightedCode}</pre>
        </div>`
    } else {
        throw new Error(`Unknown partType ${part.partType}`)
    }
}