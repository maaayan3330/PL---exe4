function createSum() {
    // please implement this method.
    let sum_counter = 0;
    return function sum(x) {
        if (arguments.length <= 0) {
            return sum_counter;
        }
        sum_counter = sum_counter + x;
        return sum_counter;
    }
  }
  