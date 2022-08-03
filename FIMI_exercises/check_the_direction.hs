{-
Em có 1 bài ko khó lắm về mặt syntax nhưng hơi challenge về mặt giải quyết vấn đề. Các bác thử xem nhé:

Bạn sống ở thành phố Cartesia, nơi mọi con đường đều được bố trí trong một mạng lưới hoàn hảo. (theo dạng block đều nhau)
Bạn đã đến quá sớm mười phút so với cuộc hẹn,
vì vậy bạn quyết định tận dụng cơ hội để đi bộ một đoạn ngắn tham quan thành phố.
Thành phố cung cấp cho công dân của mình một Ứng dụng Đi bộ trên điện thoại của họ
- mỗi khi bạn nhấn nút, nó sẽ gửi cho bạn một danh sách các chuỗi gồm một chữ cái đại diện cho hướng đi để đi bộ
(ví dụ: ['n' = Bắc, 's' = Nam, 'w' = Tây, 'e' = Đông]). 
Bạn luôn chỉ đi bộ một khối (block) duy nhất cho mỗi chữ cái (hướng) và bạn biết bạn chỉ mất một phút để đi qua một khu phố, vì vậy, hãy tạo một hàm sẽ trả về True nếu chuyến đi bộ mà ứng dụng mang lại cho bạn sẽ khiến bạn mất đúng mười phút (bạn không muốn đến sớm hay muộn!) và tất nhiên sẽ đưa bạn trở lại điểm xuất phát.
Trả về False nếu không.

Ghi chú:
bạn sẽ luôn nhận được một List chứa một kiểu dữ liệu ngẫu nhiên các chữ cái chỉ đường (chỉ 'n', 's', 'e' hoặc 'w').
App sẽ không đưa bạn danh sách trống
-}


type Direction = Char
type Location = (Int, Int)

goToDirection :: Location -> Direction -> Location
goToDirection (x, y) d
    | d == 'n' = (x, y + 1) -- go forward
    | d == 's' = (x, y - 1) -- go backward
    | d == 'e' = (x + 1, y) -- go right
    | d == 'w' = (x - 1, y) -- go left
    | otherwise = error "invalid direction"

checkThePath :: [Direction] -> Bool
checkThePath ds 
    | length ds /= 10 = False -- check if the path takes exact 10 minutes
    | otherwise = originalLocation == destinationLocation -- check if the path brings you back to original location
    where originalLocation = (0, 0)
          destinationLocation = foldl goToDirection originalLocation ds

