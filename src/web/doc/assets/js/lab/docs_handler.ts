import { ScrollHandler } from '../controls/scroll';
import { log } from '../common/logger';
import { set_grammar, set_input } from '../common/session_storage';

export function setData(target: HTMLElement, doc: Document = document, win: Window = window) {
    let id = target.dataset.id;
    if (id) {
        console.log(id);

        let ele = null;

        if ((ele = doc.getElementById(id + "-parser"))) {
            set_input(ele.innerText, win);
        } else {
            set_input("");
        }


        if ((ele = doc.getElementById(id)))
            set_grammar(ele.innerText, win)
    }
}

/**
 * Handles the interaction between the documents browser
 * iframe and the lab page. 
*/
export default function (docs_iframe: HTMLIFrameElement, docs_host: HTMLIFrameElement) {

    if (docs_iframe.tagName == "IFRAME") {

        let handler = new ScrollHandler(docs_iframe, docs_host);

        // Try to decode url to get a reference document and location. 
        // if one does not exist, then we should load the doc page
        // using that address otherwise, load the root docs path.

        let hash = document.location.hash;

        if (hash.slice(0, 6) == "#page:") {
            let source = hash.slice(6);
            let decoded_source = atob(source);
            docs_iframe.src = decoded_source;
            docs_host.classList.add("loading");
            docs_host.classList.add("active");
        }

        log("Configuring docs integration");

        docs_iframe.addEventListener("load", w => {
            let docs_doc = docs_iframe.contentDocument;
            let docs_win = docs_iframe.contentWindow;
            if (docs_doc != null && docs_win != null) {

                let html_element = docs_doc.documentElement;
                docs_doc.body.classList.add("lab-iframe");
                html_element.style.overflow = "hidden";

                handler.set_target(html_element);

                for (const anchor of Array.from(docs_doc.getElementsByTagName("a"))) {
                    /// Update path with lab candidates path.
                    if (anchor.classList.contains("lab-candidate")) {
                        anchor.addEventListener("click", e => {
                            let docs_win = docs_iframe.contentWindow;
                            setData(<any>e.target, <any>docs_doc, <any>docs_win);
                            e.preventDefault();
                            return false;
                        });
                    } else {


                        // Prevent default action on most anchors
                        anchor.addEventListener("click", e => {
                            e.preventDefault();
                            return false;
                        });
                    }
                }

                docs_host.classList.remove("loading");
            }
        });



    } else {
        alert("Docs frame not correctly connected, cannot integrate docs");
    }
}

//#allow
window.lab_anchor_click = (anchor: AnchorElement) => {
    setData(anchor);
}