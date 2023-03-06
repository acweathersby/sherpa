import { ScrollHandler } from '.';
import { log } from './logger';

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
            docs_iframe.src = hash.slice(6).replace("/\%2f/g", "/");
        }

        log("Configuring docs integration");

        docs_iframe.addEventListener("load", w => {
            let docs_doc = docs_iframe.contentDocument;
            if (docs_doc) {
                let html_element = docs_doc.documentElement;
                docs_doc.body.classList.add("lab_iframe");
                html_element.style.overflow = "hidden";

                handler.set_target(html_element);

                // Prevent default action on most anchors
                for (const anchor of Array.from(docs_doc.getElementsByTagName("a"))) {
                    anchor.addEventListener("click", e => {
                        e.preventDefault();
                        return false;
                    });
                }

                docs_host.classList.remove("loading");
            }
        });



    } else {
        alert("Docs frame not correctly connected, cannot integrate docs");
    }


}