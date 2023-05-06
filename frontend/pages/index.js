import * as api from "../api.js"
import * as links from "../links.js"

export default async function (groups, container) {
    container.innerHTML = `<h1>Welcome to plpatterns</h1>
        <p>the ultimate repository for patterns in any programming language.Patterns are structures or templates for 
        solving common tasks or problems. For example how to read a file's content into a string or how to iterate over
        over a list.</p>
        <p><a href="#${links.search()}">Search</a> for a specific pattern or browse patterns by programming language:</p>`

    const languages = await api.loadLanguages()
    container.innerHTML += `<ul>
    ${languages.map(language => `<li><a href="#${links.language(language.name)}">${language.name}</a></li>`).join(``)}
    </ul>`

    container.innerHTML += `
    <p>All the examples on this site are taken from a repository on <a href="${import.meta.env.VITE_PATTERN_REPO}">
    Github</a>. The code for site itself can also be found on <a href="${import.meta.env.VITE_SITE_REPO}">Github</a>.If
    you feel a pattern is missing feel free to open an issue or to send a pull request.</p>`
}