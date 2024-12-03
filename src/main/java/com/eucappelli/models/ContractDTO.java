package com.eucappelli.models;

import com.eucappelli.enums.DrawnType;
import com.eucappelli.utils.Strings;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class ContractDTO {
    private String companyName;
    private String companyEmail;
    private DrawnType drawnType;
    private String startDate;
    private String endDate;
    private FileDTO contractFile;

    public String contractConfirmation() {
        return String.format("%s: %s\n%s: %s\n%s: %s\n%s: %s\n%s: %s",
                Strings.COMPANY_NAME, companyName, Strings.EMAIL, companyEmail, Strings.DRAWN_BY, drawnString(),
                Strings.START_DATE, startDate, Strings.END_DATE, endDate);
    }

    private String drawnString() {
        return drawnType == DrawnType.COMPANY ? Strings.COMPANY :
                drawnType == DrawnType.AGENCY ? Strings.AGENCY : Strings.UNKNOWN;
    }
}
