# PROGRAM KOTY WYSOKOŚCIOWE
program do wstawiania kot wysokościowych w AutoCadzie dla potrzeb projektu drogowego w polskich standardach, który działa także na starszych wersjach. 

## WYMAGANIA
Program powinien uruchamiać się na wersjach 32-bit  oraz 64-bit AutoCAD ( Program działa nie tylko w wersji AutoCAD Civil 3D). Do użytkowania programu wymagane jest wstawienie bloku dynamiczbego o nazwie :

>"Kota-mm"

blok powinien posiadać 2 stany widoczności [Visibility States.] :

    "Underline YES | Wipeout YES"

    "Underline NO  | Wipeout NO"

blok powinien posiadać także 2 atrybuty o nazwach :

    "TEXT"
    "HEIGHT"
## URUCHAMIANIANIE
Po załadowaniu plików lisp do programu do AutoCAD'a należy wpisać skrót polecenia 

|    skrót      |  funkcja       | plik|
| ------------- |:-------------:| ------------- |
| klp |  wstawianie bloku koty do poprzeczek     |KOTY-POPRZECZKI.LSP |
| jd |  wstawianie bloku koty do profilu  | KOTY-PROFIL.LSP |


Aby wyjsc z programu należy wcisnąć klawisz
>Esc
