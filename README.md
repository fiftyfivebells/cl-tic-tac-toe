# Tic Tac Toe in Common Lisp (SBCL)

This is a very simple implementation, and my first attempt at building something somewhat substantial with Common Lisp. The program asks the user if they want to go first, then reacts accordingly. The computer chooses moves and then gives a brief explanation of the strategy behind its move. 

The computer is not an optimal player yet. It can block opponent wins, block certain strategies, and go for its own wins when it sees two in a row. However, it still resorts to random moves when there are no obvious strategies available.

The next step will be to improve the computer's decision-making so that it can make better choices during the earlier parts of the game. The goal is to make the computer unbeatable, so that the best result is a tie. Currently, the computer can be beaten if the user takes advantage of the computer's random moves.