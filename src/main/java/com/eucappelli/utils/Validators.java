package com.eucappelli.utils;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;

public class Validators {
    private static final String DATE_FORMAT = "dd/MM/yyyy";

    public static boolean isValidDre(String dre) {
        return dre != null && dre.length() == 9 && dre.matches("^[0-9]{9}$");
    }

    public static boolean isValidCRA(String cra) {
        return cra != null && cra.matches("^\\d{1,2}\\.\\d$") && Double.parseDouble(cra) > 0 && Double.parseDouble(cra) <= 10.0;
    }

    public static boolean isValidName(String fullName) {
        return fullName != null && fullName.trim().length() > 3 && fullName.matches("^[A-Za-zÀ-ÿ]+(?: [A-Za-zÀ-ÿ]+)+$");
    }

    public static boolean isValidEmail(String email) {
        return email != null && email.matches("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$");
    }

    public static boolean isValidDate(String date) {
        try {
            DateFormat df = new SimpleDateFormat(DATE_FORMAT);
            df.setLenient(false);
            df.parse(date);
            return true;
        } catch (ParseException e) {
            return false;
        }
    }
}
