package com.eucappelli.utils;

public class Validators {

    public static boolean isValidDre(String dre) {
        return dre != null && dre.length() == 9 && dre.matches("^[0-9]{9}$");
    }

    public static boolean isValidCRA(String cra) {
        return cra != null && cra.matches("^\\d{1,2}\\.\\d$") && Double.parseDouble(cra) > 0 && Double.parseDouble(cra) <= 10.0;
    }

    public static boolean isValidName(String fullName) {
        return fullName != null && fullName.trim().length() > 3 && fullName.matches("^[A-Za-zÀ-ÿ]+(?: [A-Za-zÀ-ÿ]+)+$");
    }
}
