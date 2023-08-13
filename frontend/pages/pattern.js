import * as api from "../api.js"
import { highlight } from "../highlight.js"
import * as cmark from "commonmark"
import { html } from "../htmlTemplate.js"

export default async function ( { languageName, alias }, container) {
    const decodedAlias = decodeURI(alias)
    const pattern = await api.loadPatternByLanguageAndAlias(languageName, decodedAlias)
    container.innerHTML = html`<h1>${pattern.name}</h1>`
    const patternHtml = await Promise.all(pattern.parts.map(part => renderPart(languageName, part)))
    container.insertAdjacentHTML("beforeend", patternHtml.join(""))
}

async function renderPart(languageName, part) {
    const renderFunctionByType = {
        "Markup": renderMarkup,
        "Code": renderCode
    }

    if (part.partType in renderFunctionByType) {
        return await renderFunctionByType[part.partType](languageName, part)
    } else {
        throw new Error(`Unknown partType ${part.partType}`)
    }
}

async function renderMarkup(languageName, part) {
    const reader = new cmark.Parser()
    const writer = new cmark.HtmlRenderer({ safe: true })
    const partHtml = writer.render(reader.parse(part.text))
    return `<div>
        <p>${partHtml}</p>
    </div>`
}

async function renderCode(languageName, part) {
    const highlightedCode = await highlight(languageName, part.text)
    return `<figure>
        <figcaption>${part.title}</figcaption>
        <pre>${highlightedCode}</pre>
    </figure>`
}