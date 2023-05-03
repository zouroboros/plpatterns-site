import * as api from "../api.js"
import * as links from "../links.js"

export default async function (groups, container) {
    container.innerHTML = `<h1>Welcome to plpatterns</h1>
        <p>the ultimate repository for patterns in any programming language</p>
        <h2>Patterns by langauge</h2>`

    const languages = await api.loadLanguages()
        container.innerHTML += `<ul>
        ${languages.map(language => `<li><a href="#${links.language(language.name)}">${language.name}</a></li>`).join(``)}
        </ul>`
}