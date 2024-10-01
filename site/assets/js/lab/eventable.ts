export class Eventable<events = {}> {

  listeners: Map<any, ((arg: any) => void)[]> = new Map

  constructor(enabled_by: Eventable<any>[] = []) {
  }

  protected data(): any { }

  protected name(): string { return ""; }

  protected emit<T extends keyof events, A = events[T], D = (arg: A) => void>(event: T, data: A) {
    for (const listener of this.listeners.get(event) ?? []) {
      (listener)(<any>data)
    }
  }

  protected haveListeners<T extends keyof events, A = events[T], D = (arg: A) => void>(event: T): boolean {
    return !!this.listeners.get(event)
  }

  addListener<T extends keyof events, A = events[T], D = (arg: A) => void>(event: T, listener: D) {
    if (!this.listeners.get(event)) {
      this.listeners.set(event, [<any>listener]);
    } else {
      this.listeners.get(event)?.push(<any>listener);
    }
  }

  removeListener<T extends keyof events, A = events[T], D = (arg: A) => void>(event: T, listener: D) {
    let listeners = this.listeners.get(event);
    if (listeners) {
      let index = listeners.findIndex(<any>listener);
      if (index > 0) {
        listeners.splice(index, 1)
      }
    }
  }
}