package com.eucappelli.models;

import com.eucappelli.utils.Strings;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class UserDTO {

    @NotBlank
    private Long id;
    private String fullName;
    private String dre;
    private Boolean isRenewal;
    private Integer requestId;
    private String cra;
    private Integer leftToComplete;
    private FileDTO boaFile;
    private FileDTO boletimFile;
    private ContractDTO contract;

    public boolean allInfoDone() {
        return fullName != null && dre != null && isRenewal != null && cra != null && leftToComplete != null;
    }

    public boolean allContractDone() {
        return contract != null && contract.getCompanyName() != null && contract.getCompanyEmail() != null &&
                contract.getDrawnType() != null && contract.getStartDate() != null && contract.getEndDate() != null;
    }

    public String getFirstName() {
        return fullName.split("\\s")[0];
    }

    public String getRenewal() {
        return isRenewal ? Strings.YES : Strings.NO;
    }

    public String infoConfirmation() {
        return String.format("%s: %s\n%s: %s\n%s %s\n%s: %s\n%s: %d", Strings.FULL_NAME,
                fullName, Strings.DRE, dre, Strings.RENEWAL, getRenewal(), Strings.CRA, cra, Strings.LEFT_TO_COMPLETE, leftToComplete);
    }
}
