import * as api from "../api.js"
import * as links from "../links.js"
import { html } from "../htmlTemplate.js"

function searchResultHtml(result) {
    const pattern = result.pattern
    return html`<li>
        <a href="#${links.pattern(pattern.language.name, pattern.alias)}">${pattern.name} (${pattern.language.name})</a>
    </li>`
}

export default async function ( { search }, container ) {
    container.innerHTML = html`<h1>Search for pattern</h1>
    <div>
        <form id="search-form">
            <input id="search-input" type="text" value="${search ?? ""}"/>
            <button type="submit">Search</button>
        </form>  
    </div>`

    const searchForm = container.querySelector("#search-form")
    searchForm.addEventListener("submit", function(event) {
        event.preventDefault()
        const searchInput = searchForm.querySelector("#search-input")
        location.hash = links.search(searchInput.value)
    })

    if (search) {
        const results = await api.loadSearchResults(search)
        container.insertAdjacentHTML("beforeend", `<ul>${results.map(searchResultHtml).join("")}</ul>`)
    }
}