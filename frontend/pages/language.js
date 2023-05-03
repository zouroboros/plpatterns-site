import * as api from "../api.js"
import * as links from "../links.js"

export default async function ({ languageName }, container) {
    container.innerHTML = `<h1>Examples for ${languageName}</h1>`
    const pattern = await api.loadPatternByLanguage(languageName)
    container.innerHTML += `<ul>
            ${pattern.map(pattern => `<li>
                <a href="#${links.pattern(languageName, pattern.name)}">${pattern.name}</a>
            </li>`).join(``)}
        </ul>`
}