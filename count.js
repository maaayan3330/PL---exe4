function countdown(n) {
    // please implement this method.
    let num = n + 1;
    return function make() {
        num = num - 1;
        return num;
    }
  }
  
  // Usage
  const timer = countdown(5);
  console.log(timer()); // 5
  console.log(timer()); // 4
  console.log(timer()); // 3