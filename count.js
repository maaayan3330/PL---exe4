function countdown(n) {
    // please implement this method.
    let num = n + 1;
    return function make() {
        num = num - 1;
        return num;
    }
  }
  
