import index from "./pages/index.js"
import language from "./pages/language.js"
import pattern from "./pages/pattern.js"
import search from "./pages/search.js"
import notFound from "./pages/notFound.js"
import error from "./pages/error.js"

const routes = {
   "^$": index,
   "^/languages/(?<languageName>[a-zA-Z]+)$": language,
    "^/pattern/(?<languageName>[a-zA-Z]+)/(?<alias>[^\n\r/]+$)": pattern,
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
        try {
            await routes[pattern](match.groups, mainElement)
        } catch (exception) {
            console.error(exception)
            await error(exception, mainElement)
        } 
        
    } else {
        await notFound(url, mainElement)
    }    
}

window.addEventListener("pageshow", (event) => navigateTo(routes, location.hash.substring(1), defaultRoute))
window.addEventListener("hashchange", (event) => navigateTo(routes, new URL(event.newURL).hash.substring(1), defaultRoute))