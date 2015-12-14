
   Useful commands:
C-x C-f                   Find file
C-x C-s                   Save file
M-x text-mode <Return>    Changes to edit text
C-f	                  Move forward a character
C-b	                  Move backward a character
M-f	                  Move forward a word
M-b	                  Move backward a word
C-n	                  Move to next line
C-p	                  Move to previous line
C-a	                  Move to beginning of line
C-e	                  Move to end of line
M-a	                  Move back to beginning of sentence
M-e	                  Move forward to end of sentence
<DEL>                     Delete the character just before the cursor
C-d   	                  Delete the next character after the cursor
M-<DEL>                   Kill the word immediately before the cursor
M-d	                  Kill the next word after the cursor
C-k                       Kill from the cursor position to end of line
M-k	                  Kill to the end of the current sentence
M-x eval-buffer           Evaluate buffer (i.e after editting .emac file)
C-<SPACE>                 Activate/deactivate highlighting
c-x k                   Kill curent buffer
C-x C-b               Show list of buffers
C-x b                 Select buffer
C-h m                 View documentation on current mode
C-h b                 Opens index of all key bindings

---
GENERAL NOTES
---
- When using a where statement, there must be at least one space  preceeding it	  
- For additional where statements, make sure the number of 
    characters to the starting expressions are consistent (i.e. 7 char lengths)
- interpreter, type in ':set prompt ghci> '
- To launch haskell interactive mode, type M-x interactve
   
 Startup Commands
ghci
    :cd C:\Users\Bonu\Documents\GitHub\Public-Samples\Haskell\BioHaskell\src
    :load Main

 

EMACS
On Windows 10:
- Download emacs from the source : http://gnu.mirror.iweb.com/emacs/windows/
- Update the .emacs file at the following location:
  or type in [M-x (find-file user-init-file)]
    emacs file: c:\users\bonu\appdata\roaming\.emacs