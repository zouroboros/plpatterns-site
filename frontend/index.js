import * as api from "./api.js"
import { highlight } from "./highlight.js"

const routes = {
   "^$": async function (groups, container) {
        container.innerHTML = `<h1>Welcome to plpatterns</h1>
            <p>the ultimate repository for patterns in any programming language</p>
            <h2>Patterns by langauge</h2>`
    
        const languages = await api.loadLanguages()
            container.innerHTML += `<ul>
            ${languages.map(language => `<li><a href="#/languages/${language.name}">${language.name}</a></li>`).join(``)}
            </ul>`
   },

   "^/languages/(?<languageName>[a-zA-Z]+)$": async function ({ languageName }, container) {
        container.innerHTML = `<h1>Examples for ${languageName}</h1>`
        const pattern = await api.loadPatternByLanguage(languageName)
        container.innerHTML += `<ul>
                ${pattern.map(pattern => `<li>
                    <a href="#/pattern/${languageName}/${pattern.name}">${pattern.name}</a>
                </li>`).join(``)}
            </ul>`
    },
    "^/pattern/(?<languageName>[a-zA-Z]+)/(?<patternName>[^\n\r/]+$)": async function ( { languageName, patternName }, container) {
        const decodedPatterName = decodeURI(patternName)
        container.innerHTML = `<h1>${decodedPatterName}</h1>`
        const pattern = await api.loadPatternByLanguageAndName(languageName, decodedPatterName)
        const highlightedCode = highlight(languageName, pattern.code)
        container.innerHTML += `<div class="snippet">
                <pre>${highlightedCode}</pre>
            </div>
            <div>
                <p>${pattern.description}</p>
            </div>`
    }
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