import { sleep } from "./pipeline";

const themes = [
  { name: "please-dont-debug", eCol1: "", eCol2: "", eCol3: "" },
  { name: "industrial-solace", eCol1: "", eCol2: "", eCol3: "" },
]


function setupSetting() {
  let setting_panel = document.querySelector("#settings-panel");

  if (!setting_panel) return;

  let close_button = <HTMLDivElement>setting_panel.querySelector(".close-button");

  let open_button = <HTMLDivElement>document.body.querySelector("#open-settings-button");
  open_button.classList.add("inactive");

  open_button.addEventListener("click", () => {
    if (setting_panel.classList.contains("inactive")) {
      setting_panel.classList.replace("inactive", "active");
      open_button.classList.replace("inactive", "active");
    } else {
      setting_panel.classList.replace("active", "inactive");
      open_button.classList.replace("active", "inactive");
    }
  })

  close_button.addEventListener("click", () => {
    setting_panel.classList.replace("active", "inactive");
    open_button.classList.replace("active", "inactive");
  })

  setupThemes(setting_panel);
}

function setupThemes(setting_panel: Element) {

  let default_theme = document.body.dataset.defaulttheme;
  let active_theme = <string>default_theme;
  //document.body.classList.add(active_theme);
  let node = <HTMLTemplateElement>document.querySelector("#theme-entry-template");

  if (!node) return;


  Array.from(setting_panel.querySelectorAll(".theme")).map(t => {
    let theme = <HTMLDivElement><any>t;
    let theme_entry = <HTMLElement>(<HTMLElement>node.content.cloneNode(true)).firstElementChild;
    let title = <HTMLElement>theme_entry.querySelector(".theme-entry-title");
    let theme_entry_theme = <HTMLElement>theme_entry.querySelector(".theme");

    let { bg, mg, fg, target: target_class } = theme.dataset;

    theme_entry.style.color = <string>fg;
    theme_entry.style.backgroundColor = <string>bg;

    theme_entry_theme.style.borderColor = <string>mg;
    theme_entry_theme.style.backgroundColor = <string>fg;

    title.innerHTML = (<string>target_class).replace(/^(\w)|-(\w)/g, " <span style='text-transform:uppercase'>$1$2</span>");

    theme_entry.addEventListener("click", async _ => {
      if (active_theme != target_class) {
        let old_theme = active_theme;
        active_theme = <string>target_class;
        document.body.classList.replace(old_theme, active_theme);
        //document.body.classList.remove(old_theme);
        await sleep(400);
      }
    });

    theme.parentElement?.replaceChild(theme_entry, theme);

    return {
      theme_entry, bg, fg, target_class
    };
  });

  console.log(themes);
}



window.addEventListener("load", setupSetting);