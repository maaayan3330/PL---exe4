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
  
  // Usage
  const sum = createSum();
  console.log(sum(10)); // 10
  console.log(sum(20)); // 30
  console.log(sum()); // 30
  console.log(sum(30));