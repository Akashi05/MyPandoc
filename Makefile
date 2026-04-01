##
## EPITECH PROJECT, 2025
## Wolfram
## File description:
## the Makefile
##

NAME = mypandoc

all:
	stack build
	cp $(shell stack path --local-install-root)/bin/$(NAME) .

clean:
	stack clean
	rm -f *~

fclean: clean
	rm -f $(NAME)

re: fclean all
