package com.eucappelli.models;

import com.eucappelli.enums.DrawnType;
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
    private FileDTO reportFile;
}
