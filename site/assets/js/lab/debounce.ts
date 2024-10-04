export class Debounce<T extends () => void> {
  callable: T
  br: number = 0
  to_id: number = -1
  time_out: number = 0
  max_time_out: number = 500
  min_time_out: number = 0

  constructor(callable: T, bounce_ratio: number = 0.25, min_time_out: number = 200) {
    this.callable = callable
    this.br = 1 + bounce_ratio
    this.min_time_out = min_time_out
  }

  call() {
    if (this.to_id < 0) {
      this.time_out = this.min_time_out
    } else {

      this.time_out *= this.br;

      this.time_out = Math.min(this.time_out, this.max_time_out)

      clearTimeout(this.to_id);
    }

    this.to_id = setTimeout(() => {
      this.to_id = -1;
      this.callable();
    }, this.time_out)
  }
}