import index from "./pages/index.js"
import language from "./pages/language.js"
import pattern from "./pages/pattern.js"
import search from "./pages/search.js"

const routes = {
   "^$": index,
   "^/languages/(?<languageName>[a-zA-Z]+)$": language,
    "^/pattern/(?<languageName>[a-zA-Z]+)/(?<patternName>[^\n\r/]+$)": pattern,
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