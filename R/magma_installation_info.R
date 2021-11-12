magma_installation_info <- function() {
    # If MAGMA still didn't install,
    # give instructions to the user on how to do this manually.
    messager(
        paste0(
            "MAGMA_Celltyping was unable to install MAGMA.\n\n",
            "Please download MAGMA manually ",
            "from https://ctg.cncr.nl/software/magma\n",
            "The executable should then be copied to /usr/local/bin\n\n",
            "Alternatively, you can download it to whereever you want",
            " and add the folder containing it to your PATH.\n",
            "That is, if you’ve placed the file in ‘~/Packages/’",
            "and you use bash (instead of e.g. zsh) then add to ",
            "‘~/.bash_profile’ this line",
            "'export PATH=~/Packages/magma:$PATH'."
        )
    )
}
