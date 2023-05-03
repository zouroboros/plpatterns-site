import * as api from "./api.js"
import * as links from "./links.js"
import { highlight } from "./highlight.js"
import * as cmark from "commonmark"
import index from "./pages/index.js"
import search from "./pages/search.js"

const routes = {
   "^$": index,

   "^/languages/(?<languageName>[a-zA-Z]+)$": async function ({ languageName }, container) {
        container.innerHTML = `<h1>Examples for ${languageName}</h1>`
        const pattern = await api.loadPatternByLanguage(languageName)
        container.innerHTML += `<ul>
                ${pattern.map(pattern => `<li>
                    <a href="#${links.pattern(languageName, pattern.name)}">${pattern.name}</a>
                </li>`).join(``)}
            </ul>`
    },
    "^/pattern/(?<languageName>[a-zA-Z]+)/(?<patternName>[^\n\r/]+$)": async function ( { languageName, patternName }, container) {
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
    },
    "^/search(/(?<search>[^\n\r/]+))?$": search
}

function defaultRoute(url, container) {
    container.innerHTML = `<h2>Sorry couldn't find this page.</h2>`
}

async function navigateTo(routes, url, defaultRoute) {
    const mainElement = document.getElementById("main")

    const matchedRoute = Object.keys(routes)
        .map(pattern => [pattern, url.match(pattern)])
        .find(([pattern, match]) => match !== null)

    if (matchedRoute !== undefined) {
        const [pattern, match] = matchedRoute
        await routes[pattern](match.groups, mainElement)
    } else {
        await defaultRoute(url, mainElement)
    }    
}

window.addEventListener("pageshow", (event) => navigateTo(routes, location.hash.substring(1), defaultRoute))
window.addEventListener("hashchange", (event) => navigateTo(routes, new URL(event.newURL).hash.substring(1), defaultRoute))