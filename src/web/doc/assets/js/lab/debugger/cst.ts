export class CSTNode {
    public children: CSTNode[]
    public name: string
    public expression: string
    public terminal: boolean;

    constructor(name: string, expression: string, terminal: boolean) {
        this.children = [];
        this.name = name;
        this.terminal = terminal;
        this.expression = expression;
    }


    toDOM(): HTMLElement {
        const ele = document.createElement("div");
        ele.classList.add("cst-node");
        ele.classList.add("close")



        const name_ele = document.createElement("div");
        name_ele.classList.add("cst-name");
        name_ele.innerText = this.name;
        ele.appendChild(name_ele);

        name_ele.addEventListener("click", e => {
            if (ele.classList.contains("open")) {
                ele.classList.add("close")
                ele.classList.remove("open")
            } else {
                ele.classList.add("open")
                ele.classList.remove("close")
            }
            e.stopImmediatePropagation();
            e.stopPropagation();
            e.preventDefault();
            return false;
        })

        if (this.terminal) {
            ele.classList.add("terminal")
        } else {
            ele.classList.add("nonterminal")
            if (this.expression) {
                let e_ele = document.createElement("span")
                e_ele.innerText = this.expression
                e_ele.classList.add("cst-rule-item-expression")
                name_ele.appendChild(e_ele);
            }
        }

        if (this.children.length > 0) {
            let children = document.createElement("div");
            children.classList.add("cst-children")
            for (const child of this.children) {
                children.appendChild(child.toDOM())
            }
            ele.appendChild(children);
        }

        return ele;
    }
}
