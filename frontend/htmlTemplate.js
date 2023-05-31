export function escapeHtml(text) {
    const paragraph = document.createElement("p")
    paragraph.innerText = text
    return paragraph.innerHTML
}

export function html(strings, ...values) {
    return String.raw({ raw: strings }, ...values.map(str => escapeHtml(str)))
}