export class FlowNode<T extends object> {
    subnodes: FlowNode<T>[];
    parent: FlowNode<T> | null = null;

    constructor(subnodes: FlowNode<T>[] = []) {
        this.subnodes = subnodes;
    }

    protected _enter(state: T) {
        this.entry(state);
        this.subnodes.forEach(n => n._enter(state));
    }

    protected _exit(state: T) {
        this.subnodes.forEach(n => n._exit(state));
        this.exit(state);
    }


    protected _update_init(transition: string, state: T) {
        this.subnodes = this.subnodes.flatMap(n => {
            if (n.parent != this) {
                n.parent = this;
                let nodes = n._update("init", state);
                if (!Array.isArray(nodes)) {
                    throw (`FlowNode update returned a an invalide type: ${typeof nodes}\n${this.update.toString()}`)
                }
                return nodes.flatMap(c => {
                    if (c == n && transition && transition != "init") {
                        return c._update(transition, state)
                    } else { return [c] }
                });
            } else {
                return [n]
            }
        });

        if (this.subnodes.some(n => n.parent != this)) {
            this._update_init(transition, state);
        }
    }


    protected _update(transition: string, state: T): FlowNode<T>[] {

        this.subnodes = this.subnodes.flatMap(n => {
            let nodes = n._update(transition, state);
            if (!Array.isArray(nodes)) {
                throw (`FlowNode update returned a an invalide type: ${typeof nodes}\n${this.update.toString()}`)
            }
            if (nodes.length < 1) {
                n._exit(state);
            }
            return nodes;
        });

        if (this.subnodes.some(n => n.parent != this)) {
            this._update_init(transition, state);
        }

        return this.update(transition, state);
    }

    public entry(state: T) { }
    public exit(state: T) { }
    public update(transition: string, state: T): FlowNode<T>[] { return [] }

    updateState(state: T) {
        if (this.parent)
            this.parent.updateState(state);

    }

    emit(transition: string) {
        if (this.parent)
            this.parent.emit(transition);

    }
}

export class RootFlowNode<T extends object> extends FlowNode<T> {
    state_id: number = 0;
    current_state: T;
    pending_transitions: [string, number][] = [];
    running: boolean = false;


    constructor(init_state: T, init_nodes: FlowNode<T>[]) {
        super(init_nodes);
        this.current_state = init_state;
        this._update_init("", init_state)
    }

    updateState(state: T) {
        this.current_state = <any>Object.fromEntries(Object.entries(state));
        this.state_id += 1;
        this.emit("StateUpdated");
    }

    emit(transition: string) {
        this.pending_transitions.push([transition, this.state_id]);
        if (!this.running) {
            window.requestAnimationFrame(_ => this._emit())
        }
    }

    _emit() {
        this.running = true;

        let transition = this.pending_transitions.shift();


        if (transition) {
            if (transition[0] == "StateUpdated" && transition[1] != this.state_id) {
                // Do nothing
            } else {
                this._update(transition[0], this.current_state);
                console.log(this.subnodes)
            }
        }

        if (this.pending_transitions.length > 0) {
            window.requestAnimationFrame(_ => this._emit())
        } else {
            this.running = false;
        }
    }
}
