/*
 * Copyright 2021 Comcast Cable Communications Management, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.automatics.rdkb.tests.snmp;

import java.util.ArrayList;
import java.util.List;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.error.ErrorType;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.constants.BroadBandSnmpConstants;
import com.automatics.rdkb.constants.BroadBandSnmpConstants.BROADBAND_WAREHOUSE_SNMP_LIST;
import com.automatics.rdkb.constants.BroadBandSnmpConstants.SNMP_MODE;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

/**
 * Class for warehouse tests
 * @author anandam.s
 *
 */
public class BroadBandSnmpWareHouseTests extends AutomaticsTestBase {

    /**
     * Verify get snmp for warehouse OIDs
     * <ol>
     * <li>Verify the SNMP MIB for emta detection 1.3.6.1.2.1.2.2.1.2.16</li>
     * <li>Verify the SNMP MIB for wireless ssid enable1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10002</li>
     * <li>Verify the SNMP MIB for wireless ssid enable 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10004</li>
     * <li>Verify the SNMP MIB for wireless ssid enable1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10006</li>
     * <li>Verify the SNMP MIB for wireless ssid enable 5g1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10104</li>
     * <li>Verify the SNMP MIB for wireless password 1.3.6.1.4.1.17270.50.2.2.3.3.1.3.10001</li>
     * <li>Verify the SNMP MIB for wireless password 5g 1.3.6.1.4.1.17270.50.2.2.3.3.1.3.10101</li>
     * <li>Verify the SNMP MIB for 2g channel 1.3.6.1.4.1.17270.50.2.2.6.1.1.18.10000</li>
     * <li>Verify the SNMP MIB for 5g channel 1.3.6.1.4.1.17270.50.2.2.6.1.1.18.10100</li>
     * <li>Verify the SNMP MIB for 2g wirekless channel 1.3.6.1.4.1.17270.50.2.2.6.1.1.3.10000</li>
     * <li>Verify the SNMP MIB for 5g wireless channel 1.3.6.1.4.1.17270.50.2.2.6.1.1.3.10100</li>
     * <li>Verify the SNMP MIB for upgrade server 1.3.6.1.2.1.69.1.3.7.0</li>
     * 
     * </ol>
     * 
     * @author anandam.s
     */

    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WH-SNMP-1000",testDecription = "Verify get snmp for warehouse OIDs")
    public void testSnmpGetOnWarehouseOIDs(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-WH-SNMP-100";
	String stepNumberStart = "s";
	String stepNumber = null;
	String snmpOutput = null;
	boolean status = false;
	String errorMessage = null;
	String hasKey = null;
	BroadBandResultObject result = new BroadBandResultObject();
	// Variable Declaration Ends
	List<BROADBAND_WAREHOUSE_SNMP_LIST> snmpListForThisTest = new ArrayList<BROADBAND_WAREHOUSE_SNMP_LIST>();

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-WH-SNMP-1000");
	LOGGER.info("TEST DESCRIPTION: Verify get snmp for warehouse OIDs");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Verify the SNMP MIB for emta detection  1.3.6.1.2.1.2.2.1.2.16");
	LOGGER.info("2. Verify the SNMP MIB for wireless ssid enable 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10004");
	LOGGER.info("3. Verify the SNMP MIB for wireless ssid enable1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10006");
	LOGGER.info("4. Verify the SNMP MIB for wireless ssid enable  5g1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10104");
	LOGGER.info("5. Verify the SNMP MIB for wireless password 1.3.6.1.4.1.17270.50.2.2.3.3.1.3.10001");
	LOGGER.info("6. Verify the SNMP MIB for wireless password  5g 1.3.6.1.4.1.17270.50.2.2.3.3.1.3.10101");
	LOGGER.info("7. Verify the SNMP MIB for 2g channel 1.3.6.1.4.1.17270.50.2.2.6.1.1.18.10000");
	LOGGER.info("8. Verify the SNMP MIB for 5g channel 1.3.6.1.4.1.17270.50.2.2.6.1.1.18.10100");
	LOGGER.info("9. Verify the SNMP MIB for 2g wirekless channel 1.3.6.1.4.1.17270.50.2.2.6.1.1.3.10000");
	LOGGER.info("10. Verify the SNMP MIB for 5g wireless channel 1.3.6.1.4.1.17270.50.2.2.6.1.1.3.10100");
	LOGGER.info("11.Verify the SNMP MIB for upgrade server 1.3.6.1.2.1.69.1.3.7.0");
	LOGGER.info("#######################################################################################");

	try {
	    for (BROADBAND_WAREHOUSE_SNMP_LIST warehouseSnmp : BROADBAND_WAREHOUSE_SNMP_LIST.values()) {
		if (warehouseSnmp.getMode().equals(SNMP_MODE.GET) || warehouseSnmp.getMode().equals(SNMP_MODE.SET_GET)) {
		    snmpListForThisTest.add(warehouseSnmp);
		}
	    }
	    if (snmpListForThisTest != null && !snmpListForThisTest.isEmpty()) {
		for (BROADBAND_WAREHOUSE_SNMP_LIST warehouseSnmp : snmpListForThisTest) {
		    status = false;
		    int index = snmpListForThisTest.indexOf(warehouseSnmp);
		    stepNumber = stepNumberStart + String.valueOf(index + 1);
		    LOGGER.info("##########################################################################");
		    LOGGER.info("STEP " + stepNumber + " : DESCRIPTION :Verify the SNMP MIB " + warehouseSnmp.getOid()
			    + "." + warehouseSnmp.getTableIndex());
		    LOGGER.info("STEP " + stepNumber + " : ACTION:get the OID value");
		    LOGGER.info("STEP " + stepNumber + " : EXPECTED:" + warehouseSnmp.getInfo()
			    + " should return non null value");
		    LOGGER.info("##########################################################################");
		    // Issue SNMP walk command for SysDescr
		    if (warehouseSnmp.getInfo().equalsIgnoreCase(BroadBandTestConstants.STRING_EMTA_DETECTION)
			    && DeviceModeHandler.isBusinessClassDevice(device)) {
			LOGGER.info("STEP " + stepNumber + " : ACTUAL :" + warehouseSnmp.getInfo()
				+ " IS NOT APPLICABLE FOR BUSINESS CLASS DEVICES");
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber,
				ExecutionStatus.NOT_APPLICABLE,
				" This step is not applicable for BUSINESS CLASS DEVICES ", false);
		    } else {
			errorMessage = "Obtained Response as No such oid for snmp mib " + warehouseSnmp.getOid();
			if (warehouseSnmp.getInfo().equalsIgnoreCase(BroadBandTestConstants.STRING_EMTA_DETECTION)
				&& DeviceModeHandler.isFibreDevice(device)) {
			    snmpOutput = CommonMethods.snmpGetOnEstb(tapEnv, device, warehouseSnmp.getOid(),
				    warehouseSnmp.getTableIndex());
			} else {
			    snmpOutput = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
				    warehouseSnmp.getOid(), warehouseSnmp.getTableIndex());
			}
			if (CommonMethods.isNotNull(snmpOutput)
				&& !snmpOutput.contains(BroadBandTestConstants.NO_SUCH_OBJECT_AVAILABLE)
				&& !snmpOutput.contains(BroadBandTestConstants.NO_SUCH_INSTANCE)
				&& !snmpOutput.toLowerCase().contains("timeout")) {
			    hasKey = warehouseSnmp.toString();
			    result = validateSnmpAndWebpaResponse(device, snmpOutput, hasKey);
			    status = result.isStatus();
			}
			if (status) {
			    LOGGER.info("STEP " + stepNumber + ": ACTUAL : Sucessfully Got the value for the OID "
				    + warehouseSnmp.getOid() + "." + warehouseSnmp.getTableIndex()
				    + " And also the Expected value and the actual value are same");
			} else {
			    errorMessage = "SNMP Get operation failed."
				    + (CommonMethods.patternMatcher(hasKey,
					    BroadBandTestConstants.WAREHOUSE_GENERAL_EMTA_DETECTION) ? result
					    .getErrorMessage() : "Actual value " + result.getErrorMessage()
					    + " and expected value " + warehouseSnmp.getValue() + " is different");
			    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			// Update the test result
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, ErrorType.SNMP
				+ errorMessage, false);
		    }
		}
	    } else {
		LOGGER.error("No MIBS are defined in the list for get SNMP values");
	    }

	} catch (Exception exe) {
	    status = false;
	    errorMessage = "Unable to retrieve SNMP MIB  " + exe.getMessage();
	    LOGGER.error(errorMessage);
	    // Update the test result
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, ErrorType.SNMP + errorMessage, true);
	}

    }

    /**
     * Method to get webpa response and cross validate with corresponding snmp response.
     *
     *
     * @param device
     *            , Dut box instance
     * @param snmpOutput
     *            , snmp response
     * @param hasKey
     *            , key in webParamMap
     * @return result instance of {@link BroadBandResultObject}
     * 
     */
    public static BroadBandResultObject validateSnmpAndWebpaResponse(Dut device, String snmpOutput, String hasKey) {
	boolean status = false;
	String errorMessage = null;
	String webpaResponse = null;
	String expectedValue = null;
	BroadBandResultObject result = new BroadBandResultObject();
	try {
	    switch (hasKey) {
	    // for emta detection webparameter not available,so verifying valid response or not.
	    case BroadBandTestConstants.WAREHOUSE_GENERAL_EMTA_DETECTION:
		if (DeviceModeHandler.isFibreDevice(device)) {
		    status = CommonMethods.patternMatcher(snmpOutput,
			    BroadBandSnmpConstants.SNMP_RESPONSE_FOR_EMTA_DETECTION_FIBER_DEVICES);
		    errorMessage = "For Fiber Devices Actual :" + snmpOutput + " Expected :"
			    + BroadBandSnmpConstants.SNMP_RESPONSE_FOR_EMTA_DETECTION_FIBER_DEVICES;
		} else {
		    status = CommonMethods.patternMatcher(snmpOutput,
			    BroadBandSnmpConstants.SNMP_RESPONSE_FOR_EMTA_DETECTION_NON_FIBER_DEVICES);
		    errorMessage = "For Non Fiber Devices Actual :" + snmpOutput + " Expected :"
			    + BroadBandSnmpConstants.SNMP_RESPONSE_FOR_EMTA_DETECTION_NON_FIBER_DEVICES;
		}
		result.setErrorMessage(errorMessage);
		break;
	    // for other steps,snmp response cross validating with webpa as well as valid response or not is checked.
	    case BroadBandTestConstants.WAREHOUSE_WIRELESS_SSID_ENABLE_XHS_2_4:
		webpaResponse = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.webParamMap.get(hasKey));
		if (CommonMethods.isNotNull(webpaResponse)) {
		    expectedValue = webpaResponse.equals(BroadBandTestConstants.TRUE) ? BroadBandTestConstants.STRING_CONSTANT_1
			    : BroadBandTestConstants.STRING_CONSTANT_2;
		    status = CommonMethods.patternMatcher(snmpOutput, expectedValue);
		}
		result.setErrorMessage(expectedValue);
		break;

	    case BroadBandTestConstants.WAREHOUSE_WIRELESS_SSID1_ENABLE_LNF_2_4:
		webpaResponse = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.webParamMap.get(hasKey));
		if (CommonMethods.isNotNull(webpaResponse)) {
		    expectedValue = webpaResponse.equals(BroadBandTestConstants.TRUE) ? BroadBandTestConstants.STRING_CONSTANT_1
			    : BroadBandTestConstants.STRING_CONSTANT_2;
		    status = CommonMethods.patternMatcher(snmpOutput, expectedValue);
		}
		result.setErrorMessage(expectedValue);
		break;

	    case BroadBandTestConstants.WAREHOUSE_WIRELESS_SSID2_ENABLE_LNF_2_4:
		webpaResponse = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.webParamMap.get(hasKey));
		if (CommonMethods.isNotNull(webpaResponse)) {
		    expectedValue = webpaResponse.equals(BroadBandTestConstants.TRUE) ? BroadBandTestConstants.STRING_CONSTANT_1
			    : BroadBandTestConstants.STRING_CONSTANT_2;
		    status = CommonMethods.patternMatcher(snmpOutput, expectedValue);
		}
		result.setErrorMessage(expectedValue);
		break;
	    case BroadBandTestConstants.WAREHOUSE_WIRELESS_SSID_ENABLE_LNF_5G:
		webpaResponse = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.webParamMap.get(hasKey));
		if (CommonMethods.isNotNull(webpaResponse)) {
		    expectedValue = webpaResponse.equals(BroadBandTestConstants.TRUE) ? BroadBandTestConstants.STRING_CONSTANT_1
			    : BroadBandTestConstants.STRING_CONSTANT_2;
		    status = CommonMethods.patternMatcher(snmpOutput, expectedValue);
		}
		result.setErrorMessage(expectedValue);
		break;
	    case BroadBandTestConstants.WAREHOUSE_WIRELESS_SSID_PASSWORD_PRIVATE_2G:
		webpaResponse = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.webParamMap.get(hasKey));
		if (CommonMethods.isNotNull(webpaResponse)) {
		    status = CommonMethods.patternMatcher(snmpOutput, webpaResponse);
		}
		result.setErrorMessage(webpaResponse);
		break;

	    case BroadBandTestConstants.WAREHOUSE_WIRELESS_SSID_PASSWORD_PRIVATE_5G:
		webpaResponse = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.webParamMap.get(hasKey));
		if (CommonMethods.isNotNull(webpaResponse)) {
		    status = CommonMethods.patternMatcher(snmpOutput, webpaResponse);
		}
		result.setErrorMessage(webpaResponse);
		break;
	    case BroadBandTestConstants.WAREHOUSE_WIFI_2G_CHANNEL:
		webpaResponse = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.webParamMap.get(hasKey));
		if (CommonMethods.isNotNull(webpaResponse)) {
		    status = CommonMethods.patternMatcher(snmpOutput, webpaResponse);
		}
		result.setErrorMessage(webpaResponse);
		break;
	    case BroadBandTestConstants.WAREHOUSE_WIFI_5G_CHANNEL:
		webpaResponse = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.webParamMap.get(hasKey));
		if (CommonMethods.isNotNull(webpaResponse)) {
		    status = CommonMethods.patternMatcher(snmpOutput, webpaResponse);
		}
		result.setErrorMessage(webpaResponse);
		break;
	    case BroadBandTestConstants.WAREHOUSE_WIFI_2G_WIRELESS_CHANNEL:
		webpaResponse = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.webParamMap.get(hasKey));
		if (CommonMethods.isNotNull(webpaResponse) && snmpOutput.equals(BroadBandTestConstants.STRING_ZERO)) {
		    expectedValue = BroadBandTestConstants.TRUE;
		    status = CommonMethods.patternMatcher(webpaResponse, expectedValue);
		} else if (CommonMethods.isNotNull(webpaResponse)
			&& !snmpOutput.equals(BroadBandTestConstants.STRING_ZERO)
			&& webpaResponse.equals(BroadBandTestConstants.FALSE)) {
		    expectedValue = tapEnv.executeWebPaCommand(device,
			    BroadBandWebPaConstants.WEBPA_WAREHOUSE_WIFI_2G_CHANNEL);
		    status = CommonMethods.patternMatcher(snmpOutput, expectedValue);
		}
		result.setErrorMessage(expectedValue);
		break;
	    case BroadBandTestConstants.WAREHOUSE_WIFI_5G_WIRELESS_CHANNEL:
		webpaResponse = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.webParamMap.get(hasKey));
		if (CommonMethods.isNotNull(webpaResponse) && snmpOutput.equals(BroadBandTestConstants.STRING_ZERO)) {
		    expectedValue = BroadBandTestConstants.TRUE;
		    status = CommonMethods.patternMatcher(webpaResponse, expectedValue);
		} else if (CommonMethods.isNotNull(webpaResponse)
			&& !snmpOutput.equals(BroadBandTestConstants.STRING_ZERO)
			&& webpaResponse.equals(BroadBandTestConstants.FALSE)) {
		    expectedValue = tapEnv.executeWebPaCommand(device,
			    BroadBandWebPaConstants.WEBPA_WAREHOUSE_WIFI_5G_CHANNEL);
		    status = CommonMethods.patternMatcher(snmpOutput, expectedValue);
		}
		result.setErrorMessage(expectedValue);
		break;
	    case BroadBandTestConstants.WAREHOUSE_UPGRADE_WITH_RESET:
		webpaResponse = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.webParamMap.get(hasKey));
		if (CommonMethods.isNotNull(webpaResponse)) {
		    status = CommonMethods.patternMatcher(snmpOutput, webpaResponse);
		}
		result.setErrorMessage(webpaResponse);
		break;

	    // If the key passed is invalid, error to be logged.
	    default:
		LOGGER.error("INVALID KEY PASSED AS PARAMETER: " + hasKey);
	    }

	}

	catch (Exception e) {
	    LOGGER.error("Exception occured in validating webpa response", e.getMessage());
	}

	result.setStatus(status);
	return result;
    }
}
