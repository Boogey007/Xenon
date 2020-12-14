import basic


print(basic.bcolors.WARNING + r"""
                          _____
_____       _____    _____\    \   _____    _____            ____       _____    _____
\    \     /    /   /    / |    | |\    \   \    \       ____\_  \__   |\    \   \    \
 \    |   |    /   /    /  /___/|  \\    \   |    |     /     /     \   \\    \   |    |
  \    \ /    /   |    |__ |___|/   \\    \  |    |    /     /\      |   \\    \  |    |
   \    |    /    |       \          \|    \ |    |   |     |  |     |    \|    \ |    |
   /    |    \    |     __/ __        |     \|    |   |     |  |     |     |     \|    |
  /    /|\    \   |\    \  /  \      /     /\      \  |     | /     /|    /     /\      \
 |____|/ \|____|  | \____\/    |    /_____/ /______/| |\     \_____/ |   /_____/ /______/|
 |    |   |    |  | |    |____/|   |      | |     | | | \_____\   | /   |      | |     | |
 |____|   |____|   \|____|   | |   |______|/|_____|/   \ |    |___|/    |______|/|_____|/
                         |___|/                         \|____|

						 """ + basic.bcolors.ENDC )
while True:
	text = input('basic > ')
	if text.strip() == "": continue
	result, error = basic.run('<stdin>', text)

	if error:
		print(error.as_string())
	elif result:
		if len(result.elements) == 1:
			print(repr(result.elements[0]))
		else:
			print(repr(result))
