
-- Question 1
-- Add the type signatures for the functions below and then remove the comments and try to compile.
-- (Use the types presented in the lecture.)

f1 :: Double -> Double -> Double -> Double -- Floating a => a -> a -> a -> a
f1 x y z = x ** (y/z)

f2 :: Double -> Double -> Double -> Double -- Floating a => a -> a -> a -> a
f2 x y z = sqrt (x/y - z)

f3 :: Bool -> Bool -> [Bool]
f3 x y = [x == True] ++ [y]

f4 :: Eq a => [a] -> [a] -> [a] -> Bool
f4 x y z = x == (y ++ z)


-- Question 2
-- Why should we define type signatures of functions? How can they help you? How can they help others?
{-

- Khi chúng ta khai báo signature của hàm, thì compiler sẽ cảnh báo chúng ta khi chúng ta viết code không khớp với signature của hàm đó. Nếu chúng ta không khai báo signature, thì compiler không thể biết chúng ta mong muốn type của inputs và outputs là gì. Điều đó có nghĩa là compiler không thể tự động giúp chúng ta phát hiện ra các lỗi sai trong quá trình khai báo hàm, nó chỉ có thể phát hiện ra các lỗi sai khi chúng ta sử dụng hàm (đôi khi là quá muộn)
VD: nếu chúng ta khai báo signature của hàm f là Int -> Int -> Int mà chúng ta lại định nghĩa hàm f là f x y = x / y là sẽ bị cảnh báo.

- Ngoài ra, signature của hàm cũng giới hạn những giá trị đầu vào mà hàm có thể nhận, điều đó giúp chúng ta kiểm soát được miền giá trị đầu vào mà người dùng có thể sử dụng. Người dùng không thể sử dụng các giá trị trái với signature để làm đầu vào cho hàm của chúng ta.
VD: nếu hàm f của chúng ta có signature là Int -> Int mà người dùng lại gọi hàm f đó với các đầu vào là String hay Bool thì sẽ không được

- Đồng thời việc cụ thể hóa signature cũng giúp cho hàm của chúng ta thực hiện được nhiều việc hơn

- Signature của hàm cũng giúp cho người khác dễ dàng nắm được một số thông tin về hàm của chúng ta mà chưa cần phải đọc code, nó giống như một bản tóm tắt cho nội dung của hàm (function definition)
VD: nếu chúng ta thấy một hàm sum với signature là sum :: Int -> Int -> Int thì chúng ta có thể dễ dàng đoán được rằng hàm này sẽ tính tổng của 2 số và 2 số đó bắt buộc phải là 2 số nguyên
VD: nếu chúng ta thấy một hàm f với signature là f :: a -> a thì chúng ta có thể chắc chắn rằng giá trị đầu ra của hàm này sẽ giống y hệt với giá trị đầu vào, vì đó là cách định nghĩa hàm duy nhất phù hợp với signature đó
-}
 

-- Question 3
-- Why should you define type signatures for variables? How can they help you?


-- Question 4
-- Are there any functions in Haskell that let you transform one type to the other? Try googling for the answer.
-- có, ví dụ hàm read :: String -> a

-- Question 5
-- Can you also define in Haskell list of lists? Did we showed any example of that? How would you access the inner
-- most elements?

listOfLists = ["Nguyen Van A", "Nguyen Van B", "Nguyen Van C"]
innerMostElement = (listOfLists !! 1) !! 2
