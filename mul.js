function createMultiplier(x) {
    // please implement this method.
    return function f(y) { return (x * y)}
  }
  






  // Usage
  const multiplyBy2 = createMultiplier(2);
  console.log(multiplyBy2(5)); // 10
  console.log(multiplyBy2(10)); // 20
  
  const multiplyBy3 = createMultiplier(3);
  console.log(multiplyBy3(4)); // 12