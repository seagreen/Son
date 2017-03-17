
# son.ebnf

## value

![value](./png/value.png)

Used by: [member](#member), [array](#array)
References: [object](#object), [array](#array), [string](#string), [number](#number), [boolean](#boolean)

## boolean

![boolean](./png/boolean.png)

Used by: [value](#value)

## object

![object](./png/object.png)

Used by: [value](#value)
References: [member](#member)

## member

![member](./png/member.png)

Used by: [object](#object)
References: [string](#string), [value](#value)

## array

![array](./png/array.png)

Used by: [value](#value)
References: [value](#value)

## number

![number](./png/number.png)

Used by: [value](#value)
References: [positiveInteger](#positiveInteger), [fraction](#fraction)

## fraction

![fraction](./png/fraction.png)

Used by: [number](#number)
References: [digit](#digit), [nonZeroDigit](#nonZeroDigit)

## positiveInteger

![positiveInteger](./png/positiveInteger.png)

Used by: [number](#number)
References: [nonZeroDigit](#nonZeroDigit), [digit](#digit)

## digit

![digit](./png/digit.png)

Used by: [fraction](#fraction), [positiveInteger](#positiveInteger)

## nonZeroDigit

![nonZeroDigit](./png/nonZeroDigit.png)

Used by: [fraction](#fraction), [positiveInteger](#positiveInteger)

## string

![string](./png/string.png)

Used by: [value](#value), [member](#member)
References: [unescaped](#unescaped), [shortcutEscape](#shortcutEscape), [codepointEscape](#codepointEscape)

## unescaped

![unescaped](./png/unescaped.png)

Used by: [string](#string)

## shortcutEscape

![shortcutEscape](./png/shortcutEscape.png)

Used by: [string](#string)

## codepointEscape

![codepointEscape](./png/codepointEscape.png)

Used by: [string](#string)
