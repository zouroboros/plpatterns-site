import * as api from "../api.js"
import { highlight } from "../highlight.js"
import * as cmark from "commonmark"
import { html } from "../htmlTemplate.js"
import copySvg from "./copy.svg";

export default async function ( { languageName, alias }, container) {
    const decodedAlias = decodeURI(alias)
    const pattern = await api.loadPatternByLanguageAndAlias(languageName, decodedAlias)
    container.innerHTML = html`<h1>${pattern.name}</h1>`

    for (let partIndex = 0; partIndex < pattern.parts.length; partIndex++) {
        const part = pattern.parts[partIndex];
        await renderPart(languageName, part, partIndex, container)
    }
}

async function renderPart(languageName, part, partId, container) {
    const renderFunctionByType = {
        "Markup": renderMarkup,
        "Code": renderCode
    }

    if (part.partType in renderFunctionByType) {
        await renderFunctionByType[part.partType](languageName, part, partId, container)
    } else {
        throw new Error(`Unknown partType ${part.partType}`)
    }
}

async function renderMarkup(languageName, part, partId, container) {
    const reader = new cmark.Parser()
    const writer = new cmark.HtmlRenderer({ safe: true })
    const partHtml = writer.render(reader.parse(part.text))
    const html = `<div>
        <p>${partHtml}</p>
    </div>`
    container.insertAdjacentHTML("beforeend", html)
}

async function renderCode(languageName, part, partId, container) {
    const highlightedCode = await highlight(languageName, part.text)
    const copyButtonId = `part-${partId}-copy-button`
    const html = `<figure>
        <figcaption>${part.title}</figcaption>
        <menu>
            <li title="Copy example to clipboard"><button type="button" id="${copyButtonId}"><img src="${copySvg}" /></button></li>
        </menu>
        <pre>${highlightedCode}</pre>
    </figure>`

    container.insertAdjacentHTML("beforeend", html)
    
    container.querySelector(`#${copyButtonId}`).addEventListener("click", function () {
        navigator.clipboard.writeText(part.text)
    })
}