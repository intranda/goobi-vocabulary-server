package io.goobi.vocabulary.service.io.excel;

import io.goobi.vocabulary.exchange.VocabularyRecord;
import io.goobi.vocabulary.model.jpa.VocabularyEntity;
import io.goobi.vocabulary.service.io.TabularRecordExporter;
import io.goobi.vocabulary.service.io.TabularRecordImporter;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.usermodel.WorkbookFactory;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

@Service
public class ExcelMapperImpl implements ExcelMapper {
    public static final int MAX_CELL_LENGTH = 32767;
    private final TabularRecordExporter tabularRecordExporter;
    private final TabularRecordImporter tabularRecordImporter;

    private Logger logger = LoggerFactory.getLogger(ExcelMapperImpl.class);

    public ExcelMapperImpl(TabularRecordExporter tabularRecordExporter, TabularRecordImporter tabularRecordImporter) {
        this.tabularRecordExporter = tabularRecordExporter;
        this.tabularRecordImporter = tabularRecordImporter;
    }

    @Override
    public void toExcel(VocabularyEntity vocabulary, OutputStream out) throws IOException {
        List<List<String>> data = tabularRecordExporter.toTabularData(vocabulary);
        try (Workbook wb = new XSSFWorkbook()) {
            Sheet sheet = wb.createSheet();
            int rowIndex = 0;
            for (List<String> row : data) {
                Row excelRow = sheet.createRow(rowIndex++);
                int columnIndex = 0;
                for (String column : row) {
                    if (column.length() > MAX_CELL_LENGTH) {
                        column = column.substring(0, MAX_CELL_LENGTH);
                    }
                    excelRow.createCell(columnIndex++).setCellValue(column);
                }
            }
            wb.write(out);
        }
    }

    @Override
    public List<VocabularyRecord> fromExcel(VocabularyEntity vocabulary, InputStream in) throws IOException {
        Workbook wb = WorkbookFactory.create(in);
        Sheet sheet = wb.getSheetAt(0);
        List<List<String>> data = new ArrayList<>(sheet.getLastRowNum()+1);
        int numberOfColumns = -1;
        for (int i = 0; i <= sheet.getLastRowNum(); i++) {
            Row row = sheet.getRow(i);
            if (row == null) {
                logger.warn("\"null\" row in Excel file, skip processing of remaining file");
                break;
            }
            List<String> rowData = new ArrayList<>(row.getLastCellNum() + 1);
            for (int j = 0; j <= row.getLastCellNum(); j++) {
                String value = null;
                Cell cell = row.getCell(j);
                if (cell != null) {
                    switch (cell.getCellType()) {
                        case STRING -> value = cell.getStringCellValue();
                        case NUMERIC -> value = String.valueOf(cell.getNumericCellValue());
                        case BLANK -> value = "";
                        default ->
                                throw new IllegalArgumentException("Unsupported cell type \"" + cell.getCellType() + "\"");
                    }

                    if (value.isBlank()) {
                        value = null;
                    }
                }
                rowData.add(value);
                if (i == 0 && value != null) {
                    numberOfColumns = j;
                }
            }
            while (rowData.size() <= numberOfColumns) {
                rowData.add(null);
            }
            if (rowData.stream().anyMatch(Objects::nonNull)) {
                data.add(rowData.subList(0, numberOfColumns+1));
            }
        }
        return tabularRecordImporter.fromTabularData(vocabulary, data);
    }
}
