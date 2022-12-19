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
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.error.ErrorType;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.tests.snmp.BroadBandSnmpWareHouseTests;
import com.automatics.rdkb.constants.BroadBandCdlConstants;
import com.automatics.rdkb.constants.BroadBandPropertyKeyConstants;
import com.automatics.rdkb.constants.BroadBandSnmpConstants;
import com.automatics.rdkb.constants.BroadBandSnmpConstants.BROADBAND_WAREHOUSE_DOCSIS_SNMP_LIST;
import com.automatics.rdkb.constants.BroadBandSnmpConstants.BROADBAND_WAREHOUSE_SNMP_LIST;
import com.automatics.rdkb.constants.BroadBandSnmpConstants.SNMP_MODE;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.WebPaParamConstants.WebPaDataTypes;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandPostConditionUtils;
import com.automatics.rdkb.utils.BroadBandPreConditionUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.cdl.BroadBandCodeDownloadUtils;
import com.automatics.rdkb.utils.cdl.BroadBandXconfCdlUtils;
import com.automatics.rdkb.utils.cdl.FirmwareDownloadUtils;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpMib;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.snmp.SnmpDataType;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

public class BroadBandSnmpWareHouseOIDTest extends AutomaticsTestBase {

    /**
     * 
     * 
     * Test case is created as part of verify snmp set for warehouse OID
     * <ol>
     * <li>PRE CONDITION 1:Get the previous values of the OIDS</li>
     * <li>Verify set for the SNMP MIB 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10002</li>
     * <li>Verify set for the SNMP MIB 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10004</li>
     * <li>Verify set for the SNMP MIB 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10006</li>
     * <li>Verify set for the SNMP MIB 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10104</li>
     * <li>Verify set for the SNMP MIB 1.3.6.1.4.1.17270.50.2.2.3.3.1.3.10001</li>
     * <li>Verify set for the SNMP MIB 1.3.6.1.4.1.17270.50.2.2.3.3.1.3.10101</li>
     * <li>Verify set for the SNMP MIB 1.3.6.1.4.1.17270.50.2.2.6.1.1.3.10000</li>
     * <li>Verify set for the SNMP MIB 1.3.6.1.4.1.17270.50.2.2.6.1.1.3.10100</li>
     * <li>Verify set for the SNMP MIB 1.3.6.1.2.1.69.1.3.7.0 for upgrade server</li>
     * <li>POST-CONDITION : Set the values back as previous ones</li>
     * </ol>
     * 
     * @author anandam.s
     * @Refactor Sruthi Santhosh
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WH-SNMP-1002")
    public void testSnmpSetOnWareHouseOIDs(Dut device) {
	// Variable Declaration begins
	String testCaseId = "TC-RDKB-WH-SNMP-102";
	String stepNumberStart = "s";
	String stepNumber = null;
	boolean status = false;
	String errorMessage = null;
	String hasKey = null;
	BroadBandResultObject result = null;
	String defaultChannel2Ghz = null;
	String defaultChannel5Ghz = null;
	String snmpOutput = null;
	String valueToSet = null;
	HashMap<String, String> defaultChannnelValuesMap = null;
	// Variable Declaration Ends
	List<BROADBAND_WAREHOUSE_SNMP_LIST> snmpListForThisTest = new ArrayList<BROADBAND_WAREHOUSE_SNMP_LIST>();
	Map<String, String> preConditionMap = new HashMap<String, String>();

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-WH-SNMP-1002");
	LOGGER.info("TEST DESCRIPTION: verify snmp set for warehouse OID");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("PRE-CONDITION 1 : DESCRIPTION : GET THE DEFAULT CHANNEL VALUES");
	LOGGER.info("PRE-CONDITION 2 : DESCRIPTION : SET THE CHANNEL SELECTION MODE TO MANUAL FOR 2.4 AND 5 GHz ");
	LOGGER.info("PRE-CONDITION 3: DESCRIPTION :Get the previous values of the OIDS ");
	LOGGER.info("1. Verify set for the SNMP MIB 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10002");
	LOGGER.info("2. Verify set for the SNMP MIB 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10004");
	LOGGER.info("3. Verify set for the SNMP MIB 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10006");
	LOGGER.info("4. Verify set for the SNMP MIB 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10104");
	LOGGER.info("5. Verify set for the SNMP MIB 1.3.6.1.4.1.17270.50.2.2.3.3.1.3.10001");
	LOGGER.info("6. Verify set for the SNMP MIB 1.3.6.1.4.1.17270.50.2.2.3.3.1.3.10101");
	LOGGER.info("7. Verify set for the SNMP MIB 1.3.6.1.4.1.17270.50.2.2.6.1.1.3.10000");
	LOGGER.info("8. Verify set for the SNMP MIB 1.3.6.1.4.1.17270.50.2.2.6.1.1.3.10100");
	LOGGER.info("9. Verify set for the SNMP MIB 1.3.6.1.2.1.69.1.3.7.0 for upgrade server");
	LOGGER.info("POST-CONDITION 1: Set the values back as previous ones");
	LOGGER.info("POST-CONDITION 2 : DESCRIPTION : SET THE DEFAULT CHANNEL VALUES");

	LOGGER.info("#######################################################################################");

	try {
	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    LOGGER.info("#############################################################");

	    /**
	     * PRECONDITION 1 : GET THE DEFAULT CHANNEL VALUES
	     */

	    LOGGER.info("#######################################################################################");
	    LOGGER.info("PRE-CONDITION 1 : DESCRIPTION : GET THE DEFAULT CHANNEL VALUES");
	    LOGGER.info("PRE-CONDITION 1 : ACTION : GET THE DEFAULT CHANNEL VALUES USING WEBPA");
	    LOGGER.info("PRE-CONDITION 1 : EXPECTED : MUST RETRIEVE THE DEFAULT CHANNEL VALUES ");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "FAILED TO GET DEFAULT CHANNEL VALUES";
	    defaultChannnelValuesMap = BroadBandPreConditionUtils
		    .executePreconditionToGetTheDefaultChannelValues(device, tapEnv);
	    if (defaultChannnelValuesMap != null) {
		LOGGER.info("PRE-CONDITION 1: ACTUAL : DEFAULT CHANNEL VALUES ARE RETRIEVED SUCCESSFULLY");
	    } else {
		LOGGER.error("PRE-CONDITION 1 : ACTUAL : " + errorMessage);
		throw new TestException(
			BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 1: FAILED : " + errorMessage);
	    }

	    /**
	     * PRECONDITION 2 : SET THE CHANNEL SELECTION MODE TO MANUAL FOR 2.4 AND 5 GHz USING WEBPA
	     */
	    errorMessage = null;
	    status = false;

	    LOGGER.info("#######################################################################################");
	    LOGGER.info("PRE-CONDITION 2 : DESCRIPTION : SET THE CHANNEL SELECTION MODE TO MANUAL FOR 2.4 AND 5 GHz ");
	    LOGGER.info(
		    "PRE-CONDITION 2 : ACTION : SET THE CHANNEL SELECTION MODE TO MANUAL FOR 2.4 AND 5 GHz USING WEBPA ");
	    LOGGER.info(
		    "PRE-CONDITION 2 : EXPTECTED : THE CHANNEL SELECTION MODE TO MANUAL CHANGED SUCCESSFULLY FOR 2.4 AND 5 GHz");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "UNABLE TO SET THE CHANNEL SELECTION MODE TO MANUAL FOR 2.4 AND 5 GHz USING WEBPA. HENCE BLOCKING THE EXECUTION.";
	    HashMap<String, String> selectionMode = new HashMap<String, String>();
	    selectionMode.put(BroadBandWebPaConstants.WEBPA_PARAM_2_4_GHZ_CHANNEL_SELECTION_MODE,
		    BroadBandTestConstants.FALSE);
	    selectionMode.put(BroadBandWebPaConstants.WEBPA_PARAM_5_GHZ_CHANNEL_SELECTION_MODE,
		    BroadBandTestConstants.FALSE);
	    tapEnv.waitTill(BroadBandTestConstants.NINETY_SECOND_IN_MILLIS);
	    status = BroadBandWebPaUtils.executeMultipleWebpaParametersSet(device, tapEnv, selectionMode,
		    WebPaDataTypes.BOOLEAN.getValue());
	    HashMap<String, String> parameterValueMap = new HashMap<String, String>();
	    parameterValueMap.put(BroadBandWebPaConstants.WEBPA_PARAM_WIFI_2_4_APPLY_SETTING,
		    BroadBandTestConstants.TRUE);
	    parameterValueMap.put(BroadBandWebPaConstants.WEBPA_PARAM_WIFI_5_APPLY_SETTING,
		    BroadBandTestConstants.TRUE);
	    tapEnv.waitTill(BroadBandTestConstants.NINETY_SECOND_IN_MILLIS);
	    BroadBandWebPaUtils.executeMultipleWebpaParametersSet(device, tapEnv, parameterValueMap,
		    WebPaDataTypes.BOOLEAN.getValue());
	    LOGGER.info(
		    "Waiting for 90 seconds to reflect the WiFi changes before getting or setting any WiFi parameters.");
	    tapEnv.waitTill(BroadBandTestConstants.NINETY_SECOND_IN_MILLIS);
	    if (status) {
		LOGGER.info(
			"PRE-CONDITION 2 : ACTUAL : THE CHANNEL SELECTION MODE TO MANUAL FOR 2.4 AND 5 GHz CHANGED SUCCESSFULLY.");
	    } else {
		LOGGER.error("PRE-CONDITION 2: ACTUAL : " + errorMessage);
		throw new TestException(
			BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 2 : FAILED : " + errorMessage);
	    }

	    LOGGER.info("Is 2GHz enabled? " + tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_2_4_GHZ_CHANNEL_SELECTION_MODE));
	    LOGGER.info("Is 5GHz enabled? " + tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_5_GHZ_CHANNEL_SELECTION_MODE));

	    LOGGER.info("#############################################################");
	    LOGGER.info("PRE-CONDITION 3: DESCRIPTION :Get the previous values of the OIDS ");
	    LOGGER.info("PRE-CONDITION 3: ACTION :Get the previous values of the OIDS ");
	    LOGGER.info("PRE-CONDITION 3: EXPECTED : store the values");
	    LOGGER.info("#############################################################");

	    for (BROADBAND_WAREHOUSE_SNMP_LIST warehouseSnmp : BROADBAND_WAREHOUSE_SNMP_LIST.values()) {
		if (((warehouseSnmp.getMode().equals(SNMP_MODE.SET)
			|| warehouseSnmp.getMode().equals(SNMP_MODE.SET_GET))
			&& !CommonMethods.patternMatcher(warehouseSnmp.getInfo(),
				BroadBandTestConstants.FACTORY_RESET))) {
		    snmpListForThisTest.add(warehouseSnmp);
		}
	    }
	    LOGGER.info("Test executed on OIds: \n");
	    for (BROADBAND_WAREHOUSE_SNMP_LIST test : snmpListForThisTest) {
		LOGGER.info(test.toString() + " : " + test.getOid());
	    }
	    if (snmpListForThisTest != null && !snmpListForThisTest.isEmpty()) {
		// Get the previous values of the OIDS
		for (BROADBAND_WAREHOUSE_SNMP_LIST warehouseSnmp : snmpListForThisTest) {
		    // Issue SNMP get command for SysDescr
		    snmpOutput = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
			    warehouseSnmp.getOid(), warehouseSnmp.getTableIndex());
		    LOGGER.info("ACTUAL : SNMP command output for " + warehouseSnmp.toString() + " is " + snmpOutput);
		    if (CommonMethods.isNotNull(snmpOutput)
			    && !snmpOutput.contains(BroadBandSnmpConstants.SNMP_ERROR_RESPONSE_NO_OBJECT)
			    && !snmpOutput.contains(BroadBandSnmpConstants.SNMP_ERROR_RESPONSE_NO_OID) && !CommonMethods
				    .patternMatcher(snmpOutput.toLowerCase(), BroadBandTestConstants.STRING_TIMEOUT)) {
			preConditionMap.put(warehouseSnmp.getInfo(), snmpOutput);

		    } else {
			errorMessage = "Obtained null response/No such oid for  snmp mib " + warehouseSnmp.getOid();
			LOGGER.error(errorMessage);
		    }

		}
		LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");
		/**
		 * fetch the default channels In Use and get the first value for 2.4Ghz and 5Ghz
		 **/

		defaultChannel2Ghz = BroadBandWebPaUtils.getTheChannelInUseFor2or5Ghz(device, tapEnv,
			BroadBandTestConstants.BAND_2_4GHZ,
			preConditionMap.get(BroadBandTestConstants.WAREHOUSE_WIFI_2GHZ_CHANNEL));
		defaultChannel5Ghz = BroadBandWebPaUtils.getTheChannelInUseFor2or5Ghz(device, tapEnv,
			BroadBandTestConstants.BAND_5GHZ,
			preConditionMap.get(BroadBandTestConstants.WAREHOUSE_WIFI_5GHZ_CHANNEL));
		// set the OID values
		for (BROADBAND_WAREHOUSE_SNMP_LIST warehouseSnmp : snmpListForThisTest) {
		    status = false;
		    int index = snmpListForThisTest.indexOf(warehouseSnmp);
		    stepNumber = stepNumberStart + String.valueOf(index + 1);
		    /**
		     * Step-7:Set defaultChannel2Ghz value as channel value for 2Ghz Step-8:Set defaultChannel5Ghz value
		     * as channel value for 5Ghz
		     **/
		    if (CommonMethods.patternMatcher(warehouseSnmp.getInfo(),
			    BroadBandTestConstants.WAREHOUSE_WIFI_2GHZ_CHANNEL)) {
			valueToSet = defaultChannel2Ghz;
		    } else if (CommonMethods.patternMatcher(warehouseSnmp.getInfo(),
			    BroadBandTestConstants.WAREHOUSE_WIFI_5GHZ_CHANNEL)) {
			valueToSet = defaultChannel5Ghz;
		    } else {
			valueToSet = warehouseSnmp.getValue();
		    }
		    LOGGER.info("*******************************************************************************");
		    LOGGER.info("STEP " + stepNumber + " : DESCRIPTION :Verify set for the SNMP MIB "
			    + warehouseSnmp.getOid() + "." + warehouseSnmp.getTableIndex() + " for "
			    + warehouseSnmp.getInfo());
		    LOGGER.info("STEP " + stepNumber + " : ACTION:set the OID  with value " + valueToSet);
		    LOGGER.info("STEP " + stepNumber + " : EXPECTED: set should be successful for "
			    + warehouseSnmp.getInfo());
		    LOGGER.info("*******************************************************************************");
		    try {
			status = false;
			errorMessage = "Failed to set the OID" + warehouseSnmp.getOid() + "."
				+ warehouseSnmp.getTableIndex();
			if (CommonMethods.isNotNull(valueToSet)) {
			    snmpOutput = BroadBandSnmpUtils.executeSnmpSetWithTableIndexOnRdkDevices(tapEnv, device,
				    warehouseSnmp.getOid(), warehouseSnmp.getDataType(), valueToSet,
				    warehouseSnmp.getTableIndex());
			    /**
			     * Apply settings operation for setting channel value for 2.4GHz and 5GHz
			     **/
			    if (CommonMethods.patternMatcher(warehouseSnmp.getInfo(),
				    BroadBandTestConstants.WAREHOUSE_WIFI_5GHZ_CHANNEL)
				    || (CommonMethods.patternMatcher(warehouseSnmp.getInfo(),
					    BroadBandTestConstants.WAREHOUSE_WIFI_2GHZ_CHANNEL))) {
				if (CommonMethods.isNotNull(snmpOutput)
					&& !snmpOutput.contains(BroadBandSnmpConstants.SNMP_ERROR_RESPONSE_NO_OBJECT)
					&& !snmpOutput.contains(BroadBandSnmpConstants.SNMP_ERROR_RESPONSE_NO_OID)
					&& !CommonMethods.patternMatcher(snmpOutput.toLowerCase(),
						BroadBandTestConstants.STRING_TIMEOUT)) {
				    performApplysettings(device, tapEnv);
				    BroadBandCommonUtils.hasWaitForDuration(tapEnv,
					    BroadBandTestConstants.FIFTY_SECONDS_IN_MILLIS);
				}
			    }
			    if (!warehouseSnmp.getDataType().equals(SnmpDataType.HEXADECIMAL)
				    && CommonMethods.isNotNull(snmpOutput)
				    && CommonMethods.patternMatcher(snmpOutput, valueToSet)) {
				hasKey = warehouseSnmp.toString();
				result = BroadBandSnmpWareHouseTests.validateSnmpAndWebpaResponse(device, snmpOutput,
					hasKey);
				errorMessage = result.getErrorMessage();
				status = result.isStatus();
			    } else {
				String output = snmpOutput.replaceAll(BroadBandTestConstants.ESCAPE_SEQUENCE_SPACE,
					BroadBandTestConstants.EMPTY_STRING);
				LOGGER.info("output From Snmp Response is : " + output);
				errorMessage = CommonMethods.isNull(output) ? "Snmp Response is null" : output;
				status = CommonMethods.isNotNull(output)
					&& valueToSet.toLowerCase().contains(output.toLowerCase());
			    }
			}
		    } catch (Exception e) {
			status = false;
			errorMessage = "Failed to set the OID in device " + device.getHostMacAddress() + " ."
				+ e.getMessage();
			LOGGER.error("errorMessage");
		    }
		    if (status) {
			LOGGER.info("STEP " + stepNumber + ": ACTUAL : Sucessfully set the value for the OID "
				+ warehouseSnmp.getOid() + "." + warehouseSnmp.getTableIndex()
				+ " And also the Expected value and the actual value are same");
		    } else {
			errorMessage = "SNMP Set operation failed." + (CommonMethods.patternMatcher(hasKey,
				BroadBandTestConstants.WAREHOUSE_GENERAL_EMTA_DETECTION) ? errorMessage
					: "Actual value " + errorMessage + " and expected value " + valueToSet
						+ " is different");
			LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		    }
		    // Update the test result
		    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, ErrorType.SNMP + errorMessage,
			    false);
		    LOGGER.info("*******************************************************************************");
		}
	    } else {
		LOGGER.error("No MIBS are defined in the list for get/set SNMP values");
	    }
	} catch (Exception exe) {
	    status = false;
	    errorMessage = "Unable to retrieve SNMP MIB  " + exe.getMessage();
	    LOGGER.error(errorMessage);
	    // Update the test result
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, ErrorType.SNMP + errorMessage, true);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");
	    LOGGER.info("#############################################################");
	    LOGGER.info("POST-CONDITION 1: DESCRIPTION : SET THE VALUES BACK AS PREVIOUS ONES");
	    LOGGER.info("POST-CONDITION 1: ACTION : EXECUTE SNMP COMMANDS ");
	    LOGGER.info("POST-CONDITION 1: EXPECTED : VALUES SHOULD BE REVERTED TO THE PREVIOUS ONES");
	    LOGGER.info("#############################################################");
	    // set the values back as previous ones
	    for (BROADBAND_WAREHOUSE_SNMP_LIST warehouseSnmp : snmpListForThisTest) {
		status = BroadBandSnmpUtils.executeSnmpSetCommand(tapEnv, device, warehouseSnmp.getOid(),
			warehouseSnmp.getDataType(), preConditionMap.get(warehouseSnmp.getInfo()),
			warehouseSnmp.getTableIndex());
		// Apply settings operation for setting channel value for 2.4GHz
		// and 5GHz
		if (status && (CommonMethods.patternMatcher(warehouseSnmp.getInfo(),
			BroadBandTestConstants.WAREHOUSE_WIFI_5GHZ_CHANNEL)
			|| (CommonMethods.patternMatcher(warehouseSnmp.getInfo(),
				BroadBandTestConstants.WAREHOUSE_WIFI_2GHZ_CHANNEL)))) {
		    performApplysettings(device, tapEnv);
		    BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.FIFTY_SECONDS_IN_MILLIS);
		}
		if (status) {
		    LOGGER.info("POST-CONDITION 1: ACTUAL : VALUE OF OID REVERTED TO "
			    + preConditionMap.get(warehouseSnmp.getInfo()));
		} else {
		    LOGGER.error("POST-CONDITION 1: ACTUAL : VALUE OF OID NOT REVERTED TO "
			    + preConditionMap.get(warehouseSnmp.getInfo()));
		}

	    }

	    /**
	     * POST-CONDITION 2 : SET THE DEFAULT CHANNEL VALUES
	     */
	    if (defaultChannnelValuesMap != null) {

		status = false;
		errorMessage = null;
		LOGGER.info("#######################################################################################");
		LOGGER.info("POST-CONDITION 2 : DESCRIPTION : SET THE DEFAULT CHANNEL VALUES");
		LOGGER.info("POST-CONDITION 2 ACTION : SET THE DEFAULT CHANNEL VALUES USING WEBPA");
		LOGGER.info("POST-CONDITION 2: EXPECTED : MUST SET THE DEFAULT CHANNEL VALUES ");
		LOGGER.info("#######################################################################################");
		errorMessage = "FAILED TO SET DEFAULT CHANNEL VALUES";
		status = BroadBandPostConditionUtils.executePostConditionToSetTheDefaultChannelValues(device, tapEnv,
			defaultChannnelValuesMap);
		if (status) {
		    LOGGER.info("POST-CONDITION 2 : ACTUAL : SUCCESSFULLY SET THE DEFAULT CHANNEL VALUES.");
		} else {
		    LOGGER.info("POST-CONDITION 2 : ACTUAL : " + errorMessage);
		}
	    }
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}

    }

    /**
     * Method to perform apply settings after setting channel value for 2.4Ghz and 5ghz
     * 
     * @param device
     *            {@link Dut}
     * @param tapEnv
     *            {@link AutomaticsTapApi}
     * @return true if applysettings operation is success
     * @Refactor Sruthi Santhosh
     */
    public static boolean performApplysettings(Dut device, AutomaticsTapApi tapEnv) {
	LOGGER.debug("Entering method: performApplysettings");
	String response = null;
	boolean status = false;
	try {

	    response = BroadBandSnmpUtils.retrieveSnmpSetOutputWithDefaultIndexOnRdkDevices(device, tapEnv,
		    BroadBandSnmpMib.ESTB_WIFI_RESTORE_DEVICE.getOid(), SnmpDataType.INTEGER,
		    BroadBandTestConstants.STRING_VALUE_THREE);
	    status = CommonMethods.patternMatcher(response, BroadBandTestConstants.STRING_VALUE_THREE);
	    LOGGER.info(status ? "Apply settings operation is successful" : "Apply settings operation is Failed");
	} catch (Exception e) {
	    LOGGER.error("Exception occured while performing apply settings operation: " + e.getMessage());
	}
	LOGGER.debug("Exiting method: performApplysettings");
	return status;
    }

    /**
     * verify snmp set for warehouse OID
     * <ol>
     * <li>Verify set for the SNMP MIB 1.3.6.1.2.1.69.1.3.8.0 for upgrade protocol</li>
     * <li>Verify set for the SNMP MIB 1.3.6.1.2.1.69.1.3.3.0 for upgrade status</li>
     * <li>Verify set for the SNMP MIB 1.3.6.1.2.1.69.1.3.2.0 for upgrade file</li>
     * <li>Verify set for the SNMP MIB 1.3.6.1.4.1.17270.50.2.1.1.1008.0 for upgrade with reset</li>
     * </ol>
     * 
     * @author anandam.s
     * @refactor yamini.s
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WH-SNMP-1003")
    public void testSnmpSetOnWareHouseDOCSISOIDs(Dut device) {
	// Variable Declaration begins
	String testCaseId = "TC-RDKB-WH-SNMP-103";
	String stepNumberStart = "s";
	String stepNumber = null;
	boolean status = false;
	String errorMessage = null;
	String isIpv6SetResponse = null;
	String output = null;
	// Variable Declaration Ends

	List<BROADBAND_WAREHOUSE_DOCSIS_SNMP_LIST> snmpListForThisTest = new ArrayList<BROADBAND_WAREHOUSE_DOCSIS_SNMP_LIST>();
	Map<String, String> preConditionMap = new HashMap<String, String>();

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-WH-SNMP-1003");
	LOGGER.info("TEST DESCRIPTION: verify snmp set for warehouse OID");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Verify set for the SNMP MIB 1.3.6.1.2.1.69.1.3.8.0 for upgrade protocol");
	LOGGER.info("2. Verify set for the SNMP MIB 1.3.6.1.2.1.69.1.3.3.0 for upgrade status");
	LOGGER.info("3. Verify set for the SNMP MIB 1.3.6.1.2.1.69.1.3.2.0 for upgrade file");
	LOGGER.info("4. Verify set for the SNMP MIB 1.3.6.1.4.1.17270.50.2.1.1.1008.0 for upgrade with reset");

	LOGGER.info("#######################################################################################");

	try {
	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    LOGGER.info("PRE-CONDITION : DESCRIPTION :Get the previous values of the OIDS ");
	    LOGGER.info("PRE-CONDITION : ACTION :Get the previous values of the OIDS ");
	    LOGGER.info("PRE-CONDITION : EXPECTED : store the values");
	    for (BROADBAND_WAREHOUSE_DOCSIS_SNMP_LIST warehouseSnmp : BROADBAND_WAREHOUSE_DOCSIS_SNMP_LIST.values()) {
		// Adding SNMP OID's in a warehouseSnmp List.
		if (warehouseSnmp.getMode().equals(SNMP_MODE.SET) || warehouseSnmp.getMode().equals(SNMP_MODE.SET_GET)
			&& !warehouseSnmp.getInfo().equals(BroadBandTestConstants.STRING_IPV6_TYPE)) {
		    snmpListForThisTest.add(warehouseSnmp);
		}
	    }
	    LOGGER.info("Test executed on OIds: \n");
	    for (BROADBAND_WAREHOUSE_DOCSIS_SNMP_LIST test : snmpListForThisTest) {
		LOGGER.info(test.toString() + " : " + test.getOid());
	    }
	    if (snmpListForThisTest != null && !snmpListForThisTest.isEmpty()) {
		// Get the previous values of the OIDS
		for (BROADBAND_WAREHOUSE_DOCSIS_SNMP_LIST warehouseSnmp : snmpListForThisTest) {
		    // Issue SNMP get command for SysDescr
		    String snmpOutput = BroadBandSnmpUtils.snmpGetOnEcm(tapEnv, device, warehouseSnmp.getOid(),
			    warehouseSnmp.getTableIndex());
		    LOGGER.info("ACTUAL : SNMP command output for " + warehouseSnmp.toString() + " is " + snmpOutput);
		    if (CommonMethods.isNotNull(snmpOutput)
			    && !snmpOutput.contains(BroadBandSnmpConstants.SNMP_ERROR_RESPONSE_NO_OBJECT)
			    && !snmpOutput.contains(BroadBandSnmpConstants.SNMP_ERROR_RESPONSE_NO_OID)
			    && !snmpOutput.toLowerCase().contains(BroadBandTestConstants.STRING_TIMEOUT)) {
			preConditionMap.put(warehouseSnmp.getOid(), snmpOutput);

		    } else {
			errorMessage = "Obtained null response/No such oid for  snmp mib " + warehouseSnmp.getOid();
			LOGGER.error(errorMessage);
		    }

		}
		LOGGER.info("**********************************************************************************");
		// set the OID values
		for (BROADBAND_WAREHOUSE_DOCSIS_SNMP_LIST warehouseSnmp : snmpListForThisTest) {
		    int index = snmpListForThisTest.indexOf(warehouseSnmp);
		    stepNumber = stepNumberStart + String.valueOf(index + 1);
		    LOGGER.info("##########################################################################");
		    LOGGER.info("STEP " + stepNumber + " : DESCRIPTION :Verify set for the SNMP MIB "
			    + warehouseSnmp.getOid() + "." + warehouseSnmp.getTableIndex() + " for "
			    + warehouseSnmp.getInfo());
		    LOGGER.info("STEP " + stepNumber + " : ACTION:set the OID  with value " + warehouseSnmp.getValue());
		    LOGGER.info("STEP " + stepNumber + " : EXPECTED: set should be successful for "
			    + warehouseSnmp.getInfo());
		    LOGGER.info("##########################################################################");
		    try {
			boolean deviceOperation = false;
			boolean devAvailable = false;
			status = false;
			errorMessage = "Failed to set the OID" + warehouseSnmp.getOid() + "."
				+ warehouseSnmp.getTableIndex();

			devAvailable = BroadbandPropertyFileHandler.getStatusForDeviceCheck(device);
			if (devAvailable
				&& warehouseSnmp.getInfo().equals(BroadBandTestConstants.STRING_UPGRADE_SERVER)) {
			    LOGGER.info("Verify set for the SNMP MIB " + warehouseSnmp.WAREHOUSE_IPV6_TYPE.getOid()
				    + "." + warehouseSnmp.WAREHOUSE_IPV6_TYPE.getTableIndex() + " for "
				    + warehouseSnmp.WAREHOUSE_IPV6_TYPE.getInfo());

			    isIpv6SetResponse = BroadBandSnmpUtils.executeSnmpSetWithTableIndexOnRdkDevices(tapEnv,
				    device, warehouseSnmp.WAREHOUSE_IPV6_TYPE.getOid(),
				    warehouseSnmp.WAREHOUSE_IPV6_TYPE.getDataType(),
				    warehouseSnmp.WAREHOUSE_IPV6_TYPE.getValue(),
				    warehouseSnmp.WAREHOUSE_IPV6_TYPE.getTableIndex());
			    deviceOperation = CommonMethods.isNotNull(isIpv6SetResponse)
				    && isIpv6SetResponse.equals(warehouseSnmp.WAREHOUSE_IPV6_TYPE.getValue());

			    if (!deviceOperation) {
				errorMessage = "Unable to set the IPV6 type as '2' for the device";
			    } else {
				output = snmpSetOnWareHouseMib(tapEnv, device, warehouseSnmp);
				LOGGER.info("ouput is" + output);
				status = (CommonMethods.isNotNull(output) && output.equals(warehouseSnmp.getValue()))
					|| (CommonMethods.isNotNull(output)
						&& CommonUtils.patternSearchFromTargetString(
							warehouseSnmp.getValue().toLowerCase(), output.toLowerCase()));
				LOGGER.info("status is " + status);

			    }
			} else {
			    output = snmpSetOnWareHouseMib(tapEnv, device, warehouseSnmp);
			    LOGGER.info("ouput is" + output);
			    status = (CommonMethods.isNotNull(output) && output.equals(warehouseSnmp.getValue()))
				    || (CommonMethods.isNotNull(output) && CommonUtils.patternSearchFromTargetString(
					    warehouseSnmp.getValue().toLowerCase(), output.toLowerCase()));
			    LOGGER.info("status is " + status);
			}

		    } catch (Exception e) {
			status = false;

			errorMessage = "Failed to set the OID in device " + device.getHostMacAddress() + " ."
				+ e.getMessage();
			LOGGER.error("errorMessage");
		    }
		    if (status) {
			LOGGER.info("STEP " + stepNumber + ": ACTUAL : Sucessfully set the OID "
				+ warehouseSnmp.getOid() + "." + warehouseSnmp.getTableIndex() + " Actual: " + output
				+ " Expected: " + warehouseSnmp.getValue().toLowerCase());
		    } else {
			LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage + " Actual: " + output
				+ " Expected: " + warehouseSnmp.getValue().toLowerCase());
		    }
		    // Update the test result
		    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, ErrorType.SNMP + errorMessage,
			    false);
		}
	    } else {
		LOGGER.error("No MIBS are defined in the list for get/set SNMP values");
	    }
	} catch (Exception exe) {
	    status = false;
	    errorMessage = "Unable to retrieve SNMP MIB  " + exe.getMessage();
	    LOGGER.error(errorMessage);
	    // Update the test result
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, ErrorType.SNMP + errorMessage, true);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    // set the values back as previous ones
	    for (BROADBAND_WAREHOUSE_DOCSIS_SNMP_LIST warehouseSnmp : snmpListForThisTest) {
		String snmpOutput = BroadBandSnmpUtils.executeSnmpSetWithTableIndexOnRdkDevices(tapEnv, device,
			warehouseSnmp.getOid(), warehouseSnmp.getDataType(), warehouseSnmp.getValue(),
			warehouseSnmp.getTableIndex());
		if (!warehouseSnmp.getDataType().equals(SnmpDataType.HEXADECIMAL)) {
		    status = CommonMethods.isNotNull(snmpOutput) && snmpOutput.equals(warehouseSnmp.getValue());
		} else {
		    output = snmpOutput.replaceAll("\\s", "");
		    LOGGER.info("ouput : " + output);
		    status = CommonMethods.isNotNull(output)
			    && warehouseSnmp.getValue().toLowerCase().contains(output.toLowerCase());
		}
		if (status) {
		    LOGGER.info("Value of OID reverted back to " + preConditionMap.get(warehouseSnmp.getOid()));
		}

	    }
	    LOGGER.info("################### ENDING POST-CONFIGURATIONS ###################");

	}

	LOGGER.info("ENDING TEST CASE: TC-RDKB-WH-SNMP-1003");
    }

    /**
     * Utility method to execute SNMP SET command on Ware House Mibs
     * 
     * @param tapEnv
     *            The {@link AutomaticsTapApi} instance
     * @param device
     *            The device to be validated.
     * @param warehouseSnmp
     *            warehouse mibs to be set.
     * 
     * @return true , if Snmp set is successful
     */
    private static String snmpSetOnWareHouseMib(AutomaticsTapApi tapEnv, Dut device,
	    BROADBAND_WAREHOUSE_DOCSIS_SNMP_LIST warehouseSnmp) {
	String snmpOutput = null;
	boolean status = false;
	String output = null;
	snmpOutput = BroadBandSnmpUtils.executeSnmpSetWithTableIndexOnRdkDevices(tapEnv, device, warehouseSnmp.getOid(),
		warehouseSnmp.getDataType(), warehouseSnmp.getValue(), warehouseSnmp.getTableIndex());
	output = snmpOutput;

	if (!warehouseSnmp.getDataType().equals(SnmpDataType.HEXADECIMAL)) {
	    status = CommonMethods.isNotNull(output) && output.equals(warehouseSnmp.getValue());
	} else if (CommonMethods.isNotNull(snmpOutput)) {
	    output = snmpOutput.replaceAll("\\s", "");
	    LOGGER.info("ouput : " + output);
	    status = CommonMethods.isNotNull(output) && CommonUtils
		    .patternSearchFromTargetString(warehouseSnmp.getValue().toLowerCase(), output.toLowerCase());
	}
	return output;
    }

    /**
     * Method to verify Warehouse test coverage for Wireless OIDs - Sequence strict
     * 
     * <ol>
     * <li>Pre-Condition 1: Verify Warehouse test coverage for Wireless OIDs - Sequence strict.</li>
     * <li>Verify setting 2.4GHz private SSID disable via SNMP.</li>
     * <li>Verify setting 2.4GHz private SSID as enable via SNMP.</li>
     * <li>Verify the SNMP MIB for 2.4Ghzwireless ssid(hot spot) disable 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10004</li>
     * <li>Verify the SNMP MIB for 2.4GHzwireless ssid(hot spot) enable 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10004</li>
     * <li>Verify the SNMP MIB for 2.4GHzwireless ssid(Lnf) disable 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10006</li>
     * <li>Verify the SNMP MIB for 2.4GHz wireless ssid(Lnf) enable 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10006</li>
     * <li>Verify setting 5GHz private SSID disable via SNMP.</li>
     * <li>Verify setting 5GHz private SSID as enable via SNMP.</li>
     * <li>Verify the SNMP MIB for 5Ghzwireless ssid(hot spot) disable 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10104</li>
     * <li>Verify the SNMP MIB for 5GHzwireless ssid(hot spot) enable 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10104</li>
     * <li>Verify the SNMP MIB for 5GHzwireless ssid(Lnf) disable 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10106</li>
     * <li>Verify the SNMP MIB for 5GHz wireless ssid(Lnf) enable 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10106</li>
     * <li>Verify setting 2.4GHz private SSID via SNMP.</li>
     * <li>Verify setting 5GHz private SSID via SNMP.</li>
     * <li>Verify setting 2.4GHz private SSID Network password via SNMP.</li>
     * <li>Verify setting 5GHz private SSID Network password via SNMP.</li>
     * <li>Verify setting 2.4GHz private SSID Wireless password via SNMP.</li>
     * <li>Verify setting 5GHz private SSID Wireless password via SNMP.</li>
     * <li>Verify applying all above settings via SNMP.</li>
     * <li>Verify disabling 2.4GHZ radio via SNMP.</li>
     * <li>Verify enabling 2.4GHZ radio via SNMP.</li>
     * <li>Verify disabling 5GHZ radio via SNMP.</li>
     * <li>Verify enabling 5GHZ radio via SNMP.</li>
     * <li>Verify getting 2.4GHz radio channel value.</li>
     * <li>Verify getting 5GHz radio channel value.</li>
     * <li>Verify setting 2.4GHz channel via SNMP.</li>
     * <li>Verify setting 5GHz channel via SNMP.</li>
     * <li>Post-Condition 1: Verify setting all SSIDs Enable/Disable status to original value got at starting of the
     * test case.</li>
     * </ol>
     * 
     * @param device
     *            Dut instance
     * 
     * @author prashant.mishra12
     * @refactor yamini.s
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WH-SNMP-1007")
    public void testWirelessOidsSequenceOnWarehouse(Dut device) {
	// Variable declaration starts
	boolean status = false;
	String testCaseId = "TC-RDKB-WH-SNMP-107";
	String stepNum = "s1";
	String errorMessage = " Unable to get current value of SSIDs.";
	String response = "";
	BroadBandResultObject result = null;
	HashMap<BroadBandSnmpMib, String> currentWirelessOidsValue = new HashMap<BroadBandSnmpMib, String>();
	// Variable declaration ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-WH-SNMP-1007");
	LOGGER.info("TEST DESCRIPTION: Verify Warehouse test coverage for Wireless OIDs - Sequence strict.");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("Pre-Condition 1:  Verify getting current Enable/Disable status for all SSIDs and SSID, Password.");
	LOGGER.info("1. Verify setting 2.4GHz private SSID disable via SNMP.");
	LOGGER.info("2. Verify setting 2.4GHz private SSID as enable via SNMP.");
	LOGGER.info(
		"3. Verify the SNMP MIB for 2.4Ghzwireless ssid(hot spot) disable 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10004");
	LOGGER.info(
		"4. Verify the SNMP MIB for 2.4GHzwireless ssid(hot spot) enable 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10004");
	LOGGER.info(
		"5. Verify the SNMP MIB for 2.4GHzwireless ssid(Lnf) disable 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10006");
	LOGGER.info(
		"6. Verify the SNMP MIB for 2.4GHz wireless ssid(Lnf) enable 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10006");
	LOGGER.info("7. Verify setting 5GHz private SSID disable via SNMP.");
	LOGGER.info("8. Verify setting 5GHz private SSID as enable via SNMP.");
	LOGGER.info(
		"9. Verify the SNMP MIB for 5Ghzwireless ssid(hot spot) disable 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10104");
	LOGGER.info(
		"10. Verify the SNMP MIB for 5GHzwireless ssid(hot spot) enable 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10104");
	LOGGER.info(
		"11. Verify the SNMP MIB for 5GHzwireless ssid(Lnf) disable 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10106");
	LOGGER.info(
		"12. Verify the SNMP MIB for 5GHz wireless ssid(Lnf) enable 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10106");
	LOGGER.info("13. Verify setting 2.4GHz private SSID via SNMP.");
	LOGGER.info("14. Verify setting 5GHz private SSID via SNMP.");
	LOGGER.info("15. Verify setting 2.4GHz private SSID Network password via SNMP.");
	LOGGER.info("16. Verify setting 5GHz private SSID Network password via SNMP.");
	LOGGER.info("17. Verify setting 2.4GHz private SSID Wireless password via SNMP.");
	LOGGER.info("18. Verify setting 5GHz private SSID Wireless password via SNMP.");
	LOGGER.info("19. Verify applying all above settings via SNMP. ");
	LOGGER.info("20. Verify disabling 2.4GHZ radio via SNMP.");
	LOGGER.info("21. Verify enabling 2.4GHZ radio via SNMP.");
	LOGGER.info("22. Verify disabling 5GHZ radio via SNMP.");
	LOGGER.info("23. Verify enabling 5GHZ radio via SNMP.");
	LOGGER.info("24. Verify getting 2.4GHz radio channel value.");
	LOGGER.info("25. Verify getting 5GHz radio channel value.");
	LOGGER.info("26. Verify setting 2.4GHz channel via SNMP.");
	LOGGER.info("27. Verify setting 5GHz channel via SNMP.");
	LOGGER.info(
		"Post-Condition 1:  Verify setting all SSIDs Enable/Disable status to original value got at starting of the test case.");
	LOGGER.info("#######################################################################################");
	try {
	    LOGGER.info("################################# STARTING PRE-CONFIGURATIONS #############################");
	    
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "PRE-CONDITION 1: DESCRIPTION: Verify getting current Enable/Disable status for all SSIDs and SSID, Password.");
	    LOGGER.info(
		    "PRE-CONDITION 1: ACTION: Get Enable/Disable status, SSID & Password for all SSIDs and store in variable to revert in Post condition.");
	    LOGGER.info("PRE-CONDITION 1: EXPECTED: All current value of SSIDs should be store in a variable.");
	    LOGGER.info("#####################################################################################");
	    for (BroadBandSnmpMib mib : BroadBandTestConstants.WAREHOUSE_WIRELESS_SEQUENCE_OIDS_DATATYPE.keySet()) {
		response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device, mib.getOid(),
			mib.getTableIndex());
		if (CommonMethods.isNotNull(response)
			&& !response.contains(BroadBandSnmpConstants.SNMP_ERROR_RESPONSE_NO_OBJECT)
			&& !response.contains(BroadBandSnmpConstants.SNMP_ERROR_RESPONSE_NO_OID)
			&& !response.toLowerCase().contains(BroadBandTestConstants.STRING_TIMEOUT)) {
		    currentWirelessOidsValue.put(mib, response);
		}
		tapEnv.waitTill(BroadBandTestConstants.FIVE_SECONDS_IN_MILLIS);
	    }
	    status = BroadBandTestConstants.WAREHOUSE_WIRELESS_SEQUENCE_OIDS_DATATYPE.size() == currentWirelessOidsValue
		    .size();
	    if (status) {
		LOGGER.info(
			"PRE-CONDITION 1: ACTUAL: Current Enable/Disable status for all SSIDS, SSID & Passowrd are stored in variable.");
	    } else {
		LOGGER.error("PRE-CONDITION 1: ACTUAL: " + errorMessage);
		throw new Exception(BroadBandTestConstants.PRE_CONDITION_ERROR + errorMessage);
	    }
	    BroadBandPreConditionUtils.executePreConditionForVerifySnmpProcessUp(device, tapEnv,
		    BroadBandTestConstants.CONSTANT_2);
	    LOGGER.info("############################# COMPLETED PRE-CONFIGURATIONS #############################");

	    stepNum = "s1";
	    errorMessage = "Failed to Set 2.4GHz private SSID as disabled via SNMP.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Verify setting 2.4GHz private SSID disable via SNMP.");
	    LOGGER.info(
		    "STEP 1: ACTION : Execute SNMP Set command for following Mib: 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10001 & Set value as 2 and SNMP get after successful SNMP set.");
	    LOGGER.info(
		    "STEP 1: EXPECTED : SNMP set command should execute successfully & SNMP Get should return same set value.");
	    LOGGER.info("**********************************************************************************");
	    result = BroadBandSnmpUtils.snmpSetAndVerifySNMPResponseRdkDevices(device, tapEnv,
		    BroadBandSnmpMib.WIFI_2_4_SSID_STATUS.getOid(), BroadBandTestConstants.STRING_VALUE_TWO,
		    SnmpDataType.INTEGER, BroadBandSnmpMib.WIFI_2_4_SSID_STATUS.getTableIndex());
	    status = result.isStatus();
	    errorMessage = errorMessage + result.getErrorMessage();
	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : 2.4GHz private SSID disabled successfully via SNMP.");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "s2";
	    errorMessage = "Failed to Set 2.4GHz private SSID as enabled via SNMP.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION : Verify setting 2.4GHz private SSID as enable via SNMP.");
	    LOGGER.info(
		    "STEP 2: ACTION : Execute SNMP Set command for following Mib: 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10001 & Set value as 1 and SNMP get after successful SNMP set.");
	    LOGGER.info(
		    "STEP 2: EXPECTED : SNMP set command should execute successfully & SNMP Get should return same set value.");
	    LOGGER.info("**********************************************************************************");
	    result = BroadBandSnmpUtils.snmpSetAndVerifySNMPResponseRdkDevices(device, tapEnv,
		    BroadBandSnmpMib.WIFI_2_4_SSID_STATUS.getOid(), BroadBandTestConstants.STRING_VALUE_ONE,
		    SnmpDataType.INTEGER, BroadBandSnmpMib.WIFI_2_4_SSID_STATUS.getTableIndex());
	    status = result.isStatus();
	    errorMessage = errorMessage + result.getErrorMessage();
	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : 2.4GHz private SSID disabled successfully via SNMP.");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "s3";
	    errorMessage = "SNMP MIB for wireless ssid(hot Spot) disable 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10004 Failed";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 3: DESCRIPTION : Verify the SNMP MIB for 2.4Ghzwireless ssid(hot spot) disable 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10004");
	    LOGGER.info(
		    "STEP 3: ACTION : Execute SNMP Set command for following Mib: 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10004 & Set value as 2 and SNMP get after successful SNMP set.");
	    LOGGER.info(
		    "STEP 3: EXPECTED : SNMP set command should execute successfully & SNMP Get should return same set value.");
	    LOGGER.info("**********************************************************************************");
	    result = BroadBandSnmpUtils.snmpSetAndVerifySNMPResponseRdkDevices(device, tapEnv,
		    BroadBandSnmpMib.HOT_SPOT_2_4_SSID_STATUS.getOid(), BroadBandTestConstants.STRING_VALUE_TWO,
		    SnmpDataType.INTEGER, BroadBandSnmpMib.HOT_SPOT_2_4_SSID_STATUS.getTableIndex());
	    status = result.isStatus();
	    errorMessage = errorMessage + result.getErrorMessage();
	    if (status) {
		LOGGER.info("STEP 3: ACTUAL : Hot Sport Wirelsess SSID disabled successfully via SNMP.");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "s4";
	    errorMessage = "SNMP MIB for wireless ssid(hot spot) enable 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10004 Failed";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 4: DESCRIPTION : Verify the SNMP MIB for 2.4GHzwireless ssid(hot spot) enable 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10004");
	    LOGGER.info(
		    "STEP 4: ACTION : Execute SNMP Set command for following Mib: 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10004 & Set value as 1 and SNMP get after successful SNMP set.");
	    LOGGER.info(
		    "STEP 4: EXPECTED : SNMP set command should execute successfully & SNMP Get should return same set value.");
	    LOGGER.info("**********************************************************************************");
	    result = BroadBandSnmpUtils.snmpSetAndVerifySNMPResponseRdkDevices(device, tapEnv,
		    BroadBandSnmpMib.HOT_SPOT_2_4_SSID_STATUS.getOid(), BroadBandTestConstants.STRING_VALUE_ONE,
		    SnmpDataType.INTEGER, BroadBandSnmpMib.HOT_SPOT_2_4_SSID_STATUS.getTableIndex());
	    status = result.isStatus();
	    errorMessage = errorMessage + result.getErrorMessage();
	    if (status) {
		LOGGER.info("STEP 4: ACTUAL : Hot Sport Wirelsess SSID enabled successfully via SNMP.");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "s5";
	    errorMessage = "SNMP MIB for wireless ssid(Lnf) disable 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10006 Failed";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 5: DESCRIPTION : Verify the SNMP MIB for 2.4GHzwireless ssid(Lnf) disable 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10006");
	    LOGGER.info(
		    "STEP 5: ACTION : Execute SNMP Set command for following Mib: 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10006 & Set value as 2 and SNMP get after successful SNMP set.");
	    LOGGER.info(
		    "STEP 5: EXPECTED : SNMP set command should execute successfully & SNMP Get should return same set value.");
	    LOGGER.info("**********************************************************************************");
	    result = BroadBandSnmpUtils.snmpSetAndVerifySNMPResponseRdkDevices(device, tapEnv,
		    BroadBandSnmpMib.LNF_2_4_SSID_STATUS.getOid(), BroadBandTestConstants.STRING_VALUE_TWO,
		    SnmpDataType.INTEGER, BroadBandSnmpMib.LNF_2_4_SSID_STATUS.getTableIndex());
	    status = result.isStatus();
	    errorMessage = errorMessage + result.getErrorMessage();
	    if (status) {
		LOGGER.info("STEP 5: ACTUAL : Lnf SSID disabled successfully via SNMP.");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "s6";
	    errorMessage = "SNMP MIB for wireless ssid(Lnf) enable 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10006 Failed";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 6: DESCRIPTION : Verify the SNMP MIB for 2.4GHz wireless ssid(Lnf) enable 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10006");
	    LOGGER.info(
		    "STEP 6: ACTION : Execute SNMP Set command for following Mib: 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10006 & Set value as 1 and SNMP get after successful SNMP set.");
	    LOGGER.info(
		    "STEP 6: EXPECTED : SNMP set command should execute successfully & SNMP Get should return same set value.");
	    LOGGER.info("**********************************************************************************");
	    result = BroadBandSnmpUtils.snmpSetAndVerifySNMPResponseRdkDevices(device, tapEnv,
		    BroadBandSnmpMib.LNF_2_4_SSID_STATUS.getOid(), BroadBandTestConstants.STRING_VALUE_ONE,
		    SnmpDataType.INTEGER, BroadBandSnmpMib.LNF_2_4_SSID_STATUS.getTableIndex());
	    status = result.isStatus();
	    errorMessage = errorMessage + result.getErrorMessage();
	    if (status) {
		LOGGER.info("STEP 6: ACTUAL : Lnf SSID enabled successfully via SNMP.");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    LOGGER.info("**********************************************************************************");

	    stepNum = "s7";
	    errorMessage = "Failed to Set 5GHz private SSID as disabled via SNMP.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 7: DESCRIPTION : Verify setting 5GHz private SSID disable via SNMP.");
	    LOGGER.info(
		    "STEP 7: ACTION : Execute SNMP Set command for following Mib: 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10101 & Set value as 2 and SNMP get after successful SNMP set.");
	    LOGGER.info(
		    "STEP 7: EXPECTED : SNMP set command should execute successfully & SNMP Get should return same set value.");
	    LOGGER.info("**********************************************************************************");
	    result = BroadBandSnmpUtils.snmpSetAndVerifySNMPResponseRdkDevices(device, tapEnv,
		    BroadBandSnmpMib.ECM_STATUS_PRIVATE_WIFI_5_GHZ.getOid(), BroadBandTestConstants.STRING_VALUE_TWO,
		    SnmpDataType.INTEGER, BroadBandSnmpMib.ECM_STATUS_PRIVATE_WIFI_5_GHZ.getTableIndex());
	    status = result.isStatus();
	    errorMessage = errorMessage + result.getErrorMessage();
	    if (status) {
		LOGGER.info("STEP 7: ACTUAL : 5GHz private WiFi SSID is disabled successfully via SNMP.");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "s8";
	    errorMessage = "Failed to Set 5GHz private SSID as enabled via SNMP.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 8: DESCRIPTION : Verify setting 5GHz private SSID as enable via SNMP.");
	    LOGGER.info(
		    "STEP 8: ACTION : Execute SNMP Set command for following Mib: 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10101 & Set value as 1 and SNMP get after successful SNMP set.");
	    LOGGER.info(
		    "STEP 8: EXPECTED : SNMP set command should execute successfully & SNMP Get should return same set value.");
	    LOGGER.info("**********************************************************************************");
	    result = BroadBandSnmpUtils.snmpSetAndVerifySNMPResponseRdkDevices(device, tapEnv,
		    BroadBandSnmpMib.ECM_STATUS_PRIVATE_WIFI_5_GHZ.getOid(), BroadBandTestConstants.STRING_VALUE_ONE,
		    SnmpDataType.INTEGER, BroadBandSnmpMib.ECM_STATUS_PRIVATE_WIFI_5_GHZ.getTableIndex());
	    status = result.isStatus();
	    errorMessage = errorMessage + result.getErrorMessage();
	    if (status) {
		LOGGER.info("STEP 8: ACTUAL : 5GHz private WiFi SSID is enabled successfully via SNMP.");
	    } else {
		LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "s9";
	    errorMessage = "SNMP MIB for wireless 5GHz Wireless ssid(hot Spot) disable 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10104 Failed";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 9: DESCRIPTION : Verify the SNMP MIB for 5Ghz Wireless ssid(hot spot) disable 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10104");
	    LOGGER.info(
		    "STEP 9: ACTION : Execute SNMP Set command for following Mib: 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10104 & Set value as 2 and SNMP get after successful SNMP set.");
	    LOGGER.info(
		    "STEP 9: EXPECTED : SNMP set command should execute successfully & SNMP Get should return same set value.");
	    LOGGER.info("**********************************************************************************");
	    result = BroadBandSnmpUtils.snmpSetAndVerifySNMPResponseRdkDevices(device, tapEnv,
		    BroadBandSnmpMib.HOT_SPOT_5_SSID_STATUS.getOid(), BroadBandTestConstants.STRING_VALUE_TWO,
		    SnmpDataType.INTEGER, BroadBandSnmpMib.HOT_SPOT_5_SSID_STATUS.getTableIndex());
	    status = result.isStatus();
	    errorMessage = errorMessage + result.getErrorMessage();
	    if (status) {
		LOGGER.info("STEP 9: ACTUAL : HotSpot 5GHz SSID is disabled successfully via SNMP.");
	    } else {
		LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "s10";
	    errorMessage = "SNMP MIB for wireless ssid(Hot Spot) enable 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10104 Failed";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 10: DESCRIPTION : Verify the SNMP MIB for 5GHzwireless ssid(hot spot) enable 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10104");
	    LOGGER.info(
		    "STEP 10: ACTION : Execute SNMP Set command for following Mib: 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10104 & Set value as 1 and SNMP get after successful SNMP set.");
	    LOGGER.info(
		    "STEP 10: EXPECTED : SNMP set command should execute successfully & SNMP Get should return same set value.");
	    LOGGER.info("**********************************************************************************");
	    result = BroadBandSnmpUtils.snmpSetAndVerifySNMPResponseRdkDevices(device, tapEnv,
		    BroadBandSnmpMib.HOT_SPOT_5_SSID_STATUS.getOid(), BroadBandTestConstants.STRING_VALUE_ONE,
		    SnmpDataType.INTEGER, BroadBandSnmpMib.HOT_SPOT_5_SSID_STATUS.getTableIndex());
	    status = result.isStatus();
	    errorMessage = errorMessage + result.getErrorMessage();
	    if (status) {
		LOGGER.info("STEP 10: ACTUAL : HotSpot 5GHz SSID is enabled successfully via SNMP.");
	    } else {
		LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "s11";
	    errorMessage = "SNMP MIB for wireless ssid(Lnf) disable 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10106 Failed";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 11: DESCRIPTION : Verify the SNMP MIB for 5GHzwireless ssid(Lnf) disable 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10106");
	    LOGGER.info(
		    "STEP 11: ACTION : Execute SNMP Set command for following Mib: 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10106 & Set value as 2 and SNMP get after successful SNMP set.");
	    LOGGER.info(
		    "STEP 11: EXPECTED : SNMP set command should execute successfully & SNMP Get should return same set value.");
	    LOGGER.info("**********************************************************************************");
	    result = BroadBandSnmpUtils.snmpSetAndVerifySNMPResponseRdkDevices(device, tapEnv,
		    BroadBandSnmpMib.LNF_5_SSID_STATUS.getOid(), BroadBandTestConstants.STRING_VALUE_TWO,
		    SnmpDataType.INTEGER, BroadBandSnmpMib.LNF_5_SSID_STATUS.getTableIndex());
	    status = result.isStatus();
	    errorMessage = errorMessage + result.getErrorMessage();
	    if (status) {
		LOGGER.info("STEP 11: ACTUAL : Lnf 5GHz SSID disabled succesfully via SNMP.");
	    } else {
		LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "s12";
	    errorMessage = "SNMP MIB for wireless ssid(Lnf) enable 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10106 Failed";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 12: DESCRIPTION : Verify the SNMP MIB for 5GHz wireless ssid(Lnf) enable 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10106");
	    LOGGER.info(
		    "STEP 12: ACTION : Execute SNMP Set command for following Mib: 1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10106 & Set value as 1 and SNMP get after successful SNMP set.");
	    LOGGER.info(
		    "STEP 12: EXPECTED : SNMP set command should execute successfully & SNMP Get should return same set value.");
	    LOGGER.info("**********************************************************************************");
	    result = BroadBandSnmpUtils.snmpSetAndVerifySNMPResponseRdkDevices(device, tapEnv,
		    BroadBandSnmpMib.LNF_5_SSID_STATUS.getOid(), BroadBandTestConstants.STRING_VALUE_ONE,
		    SnmpDataType.INTEGER, BroadBandSnmpMib.LNF_5_SSID_STATUS.getTableIndex());
	    status = result.isStatus();
	    errorMessage = errorMessage + result.getErrorMessage();
	    if (status) {
		LOGGER.info("STEP 12: ACTUAL : Lnf 5GHz SSID enabled succesfully via SNMP.");
	    } else {
		LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "s13";
	    errorMessage = "Failed to Set 2.4GHz private SSID  via SNMP.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 13: DESCRIPTION : Verify setting 2.4GHz private SSID via SNMP.");
	    LOGGER.info(
		    "STEP 13: ACTION : Execute SNMP Set command for following Mib: 1.3.6.1.4.1.17270.50.2.2.2.1.1.3.10001 & Set value as 'test-ssid-2.4' and SNMP get after successful SNMP set.");
	    LOGGER.info(
		    "STEP 13: EXPECTED : SNMP set command should execute successfully & SNMP Get should return same set value.");
	    LOGGER.info("**********************************************************************************");
	    result = BroadBandSnmpUtils.snmpSetAndVerifySNMPResponseRdkDevices(device, tapEnv,
		    BroadBandSnmpMib.ECM_PRIVATE_WIFI_SSID_2_4.getOid(), BroadBandTestConstants.TEST_SSID_2_4,
		    SnmpDataType.STRING, BroadBandSnmpMib.ECM_PRIVATE_WIFI_SSID_2_4.getTableIndex());
	    status = result.isStatus();
	    errorMessage = errorMessage + result.getErrorMessage();
	    if (status) {
		LOGGER.info("STEP 13: ACTUAL : Private Wifi 2.4GHz SSID got set successfully via SNMP.");
	    } else {
		LOGGER.error("STEP 13: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "s14";
	    errorMessage = "Failed to Set 5GHz private SSID  via SNMP.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 14: DESCRIPTION : Verify setting 5GHz private SSID via SNMP.");
	    LOGGER.info(
		    "STEP 14: ACTION : Execute SNMP Set command for following Mib: 1.3.6.1.4.1.17270.50.2.2.2.1.1.3.10101 & Set value as 'test-ssid-5' and SNMP get after successful SNMP set.");
	    LOGGER.info(
		    "STEP 14: EXPECTED : SNMP set command should execute successfully & SNMP Get should return same set value.");
	    LOGGER.info("**********************************************************************************");
	    result = BroadBandSnmpUtils.snmpSetAndVerifySNMPResponseRdkDevices(device, tapEnv,
		    BroadBandSnmpMib.ECM_PRIVATE_WIFI_SSID_5.getOid(), BroadBandTestConstants.TEST_SSID_5,
		    SnmpDataType.STRING, BroadBandSnmpMib.ECM_PRIVATE_WIFI_SSID_5.getTableIndex());
	    status = result.isStatus();
	    errorMessage = errorMessage + result.getErrorMessage();
	    if (status) {
		LOGGER.info("STEP 14: ACTUAL : Private Wifi 5GHz SSID got set successfully via SNMP.");
	    } else {
		LOGGER.error("STEP 14: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "s15";
	    errorMessage = "Failed to Set 2.4GHz private SSID Network password via SNMP.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 15: DESCRIPTION : Verify setting 2.4GHz private SSID Network password via SNMP.");
	    LOGGER.info(
		    "STEP 15: ACTION : Execute SNMP Set command for following Mib: 1.3.6.1.4.1.17270.50.2.2.3.1.1.2.10001 & Set value as <password> and SNMP get after successful SNMP set.");
	    LOGGER.info(
		    "STEP 15: EXPECTED : SNMP set command should execute successfully & SNMP Get should return same set value.");
	    LOGGER.info("**********************************************************************************");
	    result = BroadBandSnmpUtils.snmpSetAndVerifySNMPResponseRdkDevices(device, tapEnv,
		    BroadBandSnmpMib.ECM_PRIVATE_WIFI_2_4_PASSPHRASE.getOid(),
		    BroadbandPropertyFileHandler.getSSIDPassword(), SnmpDataType.STRING,
		    BroadBandSnmpMib.ECM_PRIVATE_WIFI_2_4_PASSPHRASE.getTableIndex());
	    status = result.isStatus();
	    errorMessage = errorMessage + result.getErrorMessage();
	    if (status) {
		LOGGER.info("STEP 15: ACTUAL : Private Wifi 2.4GHz Network password got set successfully via SNMP.");
	    } else {
		LOGGER.error("STEP 15: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "s16";
	    errorMessage = "Failed to Set 5GHz private SSID Network password via SNMP.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 16: DESCRIPTION : Verify setting 5GHz private SSID Network password via SNMP.");
	    LOGGER.info(
		    "STEP 16: ACTION : Execute SNMP Set command for following Mib: 1.3.6.1.4.1.17270.50.2.2.3.1.1.2.10101 & Set value as <password> and SNMP get after successful SNMP set.");
	    LOGGER.info(
		    "STEP 16: EXPECTED : SNMP set command should execute successfully & SNMP Get should return same set value.");
	    LOGGER.info("**********************************************************************************");
	    result = BroadBandSnmpUtils.snmpSetAndVerifySNMPResponseRdkDevices(device, tapEnv,
		    BroadBandSnmpMib.ECM_PRIVATE_WIFI_5_PASSPHRASE.getOid(),
		    BroadbandPropertyFileHandler.getSSIDPassword(), SnmpDataType.STRING,
		    BroadBandSnmpMib.ECM_PRIVATE_WIFI_5_PASSPHRASE.getTableIndex());
	    status = result.isStatus();
	    errorMessage = errorMessage + result.getErrorMessage();
	    if (status) {
		LOGGER.info("STEP 16: ACTUAL : Private Wifi 5GHz Network password got set successfully via SNMP.");
	    } else {
		LOGGER.error("STEP 16: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "s17";
	    errorMessage = "Failed to Set 2.4GHz private SSID Wireless password via SNMP.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 17: DESCRIPTION : Verify setting 2.4GHz private SSID Wireless password via SNMP.");
	    LOGGER.info(
		    "STEP 17: ACTION : Execute SNMP Set command for following Mib: 1.3.6.1.4.1.17270.50.2.2.3.3.1.3.10001 & Set value as <password> and SNMP get after successful SNMP set.");
	    LOGGER.info(
		    "STEP 17: EXPECTED : SNMP set command should execute successfully & SNMP Get should return same set value.");
	    LOGGER.info("**********************************************************************************");
	    result = BroadBandSnmpUtils.snmpSetAndVerifySNMPResponseRdkDevices(device, tapEnv,
		    BroadBandSnmpMib.ECM_PRIVATE_WIFI_2_4_WIRELESSPASS.getOid(),
		    BroadbandPropertyFileHandler.getSSIDPassword(), SnmpDataType.STRING,
		    BroadBandSnmpMib.ECM_PRIVATE_WIFI_2_4_WIRELESSPASS.getTableIndex());
	    status = result.isStatus();
	    errorMessage = errorMessage + result.getErrorMessage();
	    if (status) {
		LOGGER.info("STEP 17: ACTUAL : Private Wifi 2.4GHz Wireless password got set successfully via SNMP.");
	    } else {
		LOGGER.error("STEP 17: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "s18";
	    errorMessage = "Failed to Set 5GHz private SSID Network password via SNMP.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 18: DESCRIPTION : Verify setting 5GHz private SSID Wireless password via SNMP.");
	    LOGGER.info(
		    "STEP 18: ACTION : Execute SNMP Set command for following Mib: 1.3.6.1.4.1.17270.50.2.2.3.3.1.3.10101 & Set value as <password> and SNMP get after successful SNMP set.");
	    LOGGER.info(
		    "STEP 18: EXPECTED : SNMP set command should execute successfully & SNMP Get should return same set value.");
	    LOGGER.info("**********************************************************************************");
	    result = BroadBandSnmpUtils.snmpSetAndVerifySNMPResponseRdkDevices(device, tapEnv,
		    BroadBandSnmpMib.ECM_PRIVATE_WIFI_5_WIRELESSPASS.getOid(),
		    BroadbandPropertyFileHandler.getSSIDPassword(), SnmpDataType.STRING,
		    BroadBandSnmpMib.ECM_PRIVATE_WIFI_5_WIRELESSPASS.getTableIndex());
	    status = result.isStatus();
	    errorMessage = errorMessage + result.getErrorMessage();
	    if (status) {
		LOGGER.info("STEP 18: ACTUAL : Private Wifi 5GHz Wireless password got set successfully via SNMP.");
	    } else {
		LOGGER.error("STEP 18: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "s19";
	    errorMessage = "Failed to apply all wireless setttings.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 19: DESCRIPTION : Verify applying all above settings via SNMP. ");
	    LOGGER.info(
		    "STEP 19: ACTION : Execute SNMP Set command for following Mib: 1.3.6.1.4.1.17270.50.2.2.1001.0 & Set value as 1");
	    LOGGER.info(
		    "STEP 19: EXPECTED : SNMP set command should execute successfully & SNMP Get should return same set value.");
	    LOGGER.info("**********************************************************************************");
	    response = BroadBandSnmpUtils.executeSnmpSetWithTableIndexOnRdkDevices(tapEnv, device,
		    BroadBandSnmpMib.WIFI_APPLY_SETTINGS.getOid(), SnmpDataType.INTEGER,
		    BroadBandTestConstants.STRING_VALUE_ONE, BroadBandTestConstants.STRING_ZERO);
	    LOGGER.info("Waiting for 90 seconds after applying all WiFi settings.");
	    tapEnv.waitTill(BroadBandTestConstants.NINETY_SECOND_IN_MILLIS);
	    status = CommonMethods.isNotNull(response)
		    && response.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_ONE);
	    if (status) {
		LOGGER.info("STEP 19: ACTUAL : Applied all above Wifi Settings via SNMP.");
	    } else {
		LOGGER.error("STEP 19: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "s20";
	    errorMessage = "Failed to disable 2.4GHz radio via SNMP.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 20: DESCRIPTION : Verify disabling 2.4GHZ radio via SNMP.");
	    LOGGER.info(
		    "STEP 20: ACTION : Execute SNMP Set command for following Mib: 1.3.6.1.4.1.17270.50.2.2.6.1.1.1.10000 & Set value as 1 and SNMP get after successful SNMP set.");
	    LOGGER.info(
		    "STEP 20: EXPECTED : SNMP set command should execute successfully & SNMP Get should return same set value.");
	    LOGGER.info("**********************************************************************************");
	    result = BroadBandSnmpUtils.snmpSetAndVerifySNMPResponseRdkDevices(device, tapEnv,
		    BroadBandSnmpMib.ENABLE_DISABLE_WIFI_RADIO_2_4_GHZ.getOid(),
		    BroadBandTestConstants.STRING_VALUE_ONE, SnmpDataType.INTEGER,
		    BroadBandSnmpMib.ENABLE_DISABLE_WIFI_RADIO_2_4_GHZ.getTableIndex());
	    status = result.isStatus();
	    errorMessage = errorMessage + result.getErrorMessage();
	    if (status) {
		LOGGER.info("STEP 20: ACTUAL : 2.4GHz radio disabled successfully via SNMP.");
	    } else {
		LOGGER.error("STEP 20: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "s21";
	    errorMessage = "Failed to enable 2.4GHz radio via SNMP.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 21: DESCRIPTION : Verify enabling 2.4GHZ radio via SNMP.");
	    LOGGER.info(
		    "STEP 21: ACTION : Execute SNMP Set command for following Mib: 1.3.6.1.4.1.17270.50.2.2.6.1.1.1.10000 & Set value as 3 and SNMP get after successful SNMP set.");
	    LOGGER.info(
		    "STEP 21: EXPECTED : SNMP set command should execute successfully & SNMP Get should return same set value.");
	    LOGGER.info("**********************************************************************************");
	    result = BroadBandSnmpUtils.snmpSetAndVerifySNMPResponseRdkDevices(device, tapEnv,
		    BroadBandSnmpMib.ENABLE_DISABLE_WIFI_RADIO_2_4_GHZ.getOid(),
		    BroadBandTestConstants.STRING_VALUE_THREE, SnmpDataType.INTEGER,
		    BroadBandSnmpMib.ENABLE_DISABLE_WIFI_RADIO_2_4_GHZ.getTableIndex());
	    status = result.isStatus();
	    errorMessage = errorMessage + result.getErrorMessage();
	    if (status) {
		LOGGER.info("STEP 21: ACTUAL : 2.4GHz radio enabled successfully via SNMP.");
	    } else {
		LOGGER.error("STEP 21: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "s22";
	    errorMessage = "Failed to disable 5GHz radio via SNMP.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 22: DESCRIPTION : Verify disabling 5GHZ radio via SNMP.");
	    LOGGER.info(
		    "STEP 22: ACTION : Execute SNMP Set command for following Mib: 1.3.6.1.4.1.17270.50.2.2.6.1.1.1.10100 & Set value as 1 and SNMP get after successful SNMP set.");
	    LOGGER.info(
		    "STEP 22: EXPECTED : SNMP set command should execute successfully & SNMP Get should return same set value.");
	    LOGGER.info("**********************************************************************************");
	    result = BroadBandSnmpUtils.snmpSetAndVerifySNMPResponseRdkDevices(device, tapEnv,
		    BroadBandSnmpMib.ENABLE_DISABLE_WIFI_RADIO_5_GHZ.getOid(), BroadBandTestConstants.STRING_VALUE_ONE,
		    SnmpDataType.INTEGER, BroadBandSnmpMib.ENABLE_DISABLE_WIFI_RADIO_5_GHZ.getTableIndex());
	    status = result.isStatus();
	    errorMessage = errorMessage + result.getErrorMessage();
	    if (status) {
		LOGGER.info("STEP 22: ACTUAL : 5GHz radio disabled successfully via SNMP.");
	    } else {
		LOGGER.error("STEP 22: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "s23";
	    errorMessage = "Failed to enable 5GHz radio via SNMP.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 23: DESCRIPTION : Verify enabling 5GHZ radio via SNMP.");
	    LOGGER.info(
		    "STEP 23: ACTION : Execute SNMP Set command for following Mib: 1.3.6.1.4.1.17270.50.2.2.6.1.1.1.10100 & Set value as 3 and SNMP get after successful SNMP set.");
	    LOGGER.info(
		    "STEP 23: EXPECTED : SNMP set command should execute successfully & SNMP Get should return same set value.");
	    LOGGER.info("**********************************************************************************");
	    result = BroadBandSnmpUtils.snmpSetAndVerifySNMPResponseRdkDevices(device, tapEnv,
		    BroadBandSnmpMib.ENABLE_DISABLE_WIFI_RADIO_5_GHZ.getOid(),
		    BroadBandTestConstants.STRING_VALUE_THREE, SnmpDataType.INTEGER,
		    BroadBandSnmpMib.ENABLE_DISABLE_WIFI_RADIO_5_GHZ.getTableIndex());
	    status = result.isStatus();
	    errorMessage = errorMessage + result.getErrorMessage();
	    if (status) {
		LOGGER.info("STEP 23: ACTUAL : 5GHz radio enabled successfully via SNMP.");
	    } else {
		LOGGER.error("STEP 23: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "s24";
	    errorMessage = "Failed to verify 2.4GHz radio channel value.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 24: DESCRIPTION : Verify getting 2.4GHz radio channel value.");
	    LOGGER.info(
		    "STEP 24: ACTION : Execute SNMP Get command for following Mib: 1.3.6.1.4.1.17270.50.2.2.6.1.1.18.10000 & compare with \"Device.WiFi.Radio.1.Channel\" value.");
	    LOGGER.info("STEP 24: EXPECTED : Value from SNMP Get and Webpa should be same.");
	    LOGGER.info("**********************************************************************************");
	    response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
		    BROADBAND_WAREHOUSE_SNMP_LIST.WAREHOUSE_WIFI_2_4_OID.getOid(),
		    BROADBAND_WAREHOUSE_SNMP_LIST.WAREHOUSE_WIFI_2_4_OID.getTableIndex());
	    tapEnv.waitTill(BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
	    if (CommonMethods.isNotNull(response)) {
		status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_2GHZ, response,
			BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    } else {
		errorMessage = "Failed to get 2.4GHz radio channel value using webpa.";
	    }
	    if (status) {
		LOGGER.info("STEP 24: ACTUAL : 2.4GHz radio channel value verified successfully.");
	    } else {
		LOGGER.error("STEP 24: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "s25";
	    errorMessage = "Failed to verify 5GHz radio channel value.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 25: DESCRIPTION : Verify getting 5GHz radio channel value.");
	    LOGGER.info(
		    "STEP 25: ACTION : Execute SNMP Get command for following Mib: 1.3.6.1.4.1.17270.50.2.2.6.1.1.18.10100 & compare with \"Device.WiFi.Radio.2.Channel\" value.");
	    LOGGER.info("STEP 25: EXPECTED : Value from SNMP Get and Webpa should be same.");
	    LOGGER.info("**********************************************************************************");
	    response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
		    BROADBAND_WAREHOUSE_SNMP_LIST.WAREHOUSE_WIFI_5_0_OID.getOid(),
		    BROADBAND_WAREHOUSE_SNMP_LIST.WAREHOUSE_WIFI_5_0_OID.getTableIndex());
	    tapEnv.waitTill(BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
	    LOGGER.info("Webpa process is up and running:"
		    + BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true));
	    if (CommonMethods.isNotNull(response)) {
		status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_5GHZ, response,
			BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    } else {
		errorMessage = "Failed to get 5 GHz radio channel value using webpa.";
	    }
	    if (status) {
		LOGGER.info("STEP 25: ACTUAL : 5GHz radio channel value verified successfully.");
	    } else {
		LOGGER.error("STEP 25: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "s26";
	    errorMessage = "Failed to set channel value as 0.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 26: DESCRIPTION : Verify setting 2.4GHz channel via SNMP.");
	    LOGGER.info(
		    "STEP 26: ACTION : Execute SNMP Set command for following Mib: 1.3.6.1.4.1.17270.50.2.2.6.1.1.3.10000 & Set value as 0 and SNMP get after successful SNMP set.");
	    LOGGER.info(
		    "STEP 26: EXPECTED : SNMP set command should execute successfully & SNMP Get should return same set value.");
	    LOGGER.info("**********************************************************************************");
	    result = BroadBandSnmpUtils.snmpSetAndVerifySNMPResponseRdkDevices(device, tapEnv,
		    BroadBandSnmpMib.ECM_WIFI_2_4_CHANNEL_INFO.getOid(), BroadBandTestConstants.STRING_VALUE_ZERO,
		    SnmpDataType.UNSIGNED_INTEGER, BroadBandSnmpMib.ECM_WIFI_2_4_CHANNEL_INFO.getTableIndex());
	    status = result.isStatus();
	    errorMessage = errorMessage + result.getErrorMessage();
	    if (status) {
		LOGGER.info("STEP 26: ACTUAL : Setting 2.4GHz channel via SNMP verified successfully.");
	    } else {
		LOGGER.error("STEP 26: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "s27";
	    errorMessage = "Failed to set channel value as 0.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 27: DESCRIPTION : Verify setting 5GHz channel via SNMP.");
	    LOGGER.info(
		    "STEP 27: ACTION : Execute SNMP Set command for following Mib: 1.3.6.1.4.1.17270.50.2.2.6.1.1.3.10100 & Set value as 0 and SNMP get after successful SNMP set.");
	    LOGGER.info(
		    "STEP 27: EXPECTED : SNMP set command should execute successfully & SNMP Get should return same set value.");
	    LOGGER.info("**********************************************************************************");
	    result = BroadBandSnmpUtils.snmpSetAndVerifySNMPResponseRdkDevices(device, tapEnv,
		    BroadBandSnmpMib.ECM_WIFI_5_CHANNEL_INFO.getOid(), BroadBandTestConstants.STRING_VALUE_ZERO,
		    SnmpDataType.UNSIGNED_INTEGER, BroadBandSnmpMib.ECM_WIFI_5_CHANNEL_INFO.getTableIndex());
	    status = result.isStatus();
	    errorMessage = errorMessage + result.getErrorMessage();
	    if (status) {
		LOGGER.info("STEP 27: ACTUAL : Setting 5GHz channel via SNMP verified successfully.");
	    } else {
		LOGGER.error("STEP 27: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	} catch (Exception exception) {
	    errorMessage = errorMessage + exception.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");
	    status = false;
	    errorMessage = "Unable to revert all SSIDs values to initial value.";
	    int count = 0;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "POST-CONDITION : DESCRIPTION : Verify setting all SSIDs Enable/Disable status to original value got at starting of the test case.");
	    LOGGER.info(
		    "POST-CONDITION : ACTION : Set all SSIDs enable/disable status as per stored in variable in pre-condition.");
	    LOGGER.info(
		    "POST-CONDITION : EXPECTED : All SSIDs enable/disable status, SSID & passwords should be set to original value(Values at the starting of the test case).");
	    LOGGER.info("#####################################################################################");
	    Iterator<Map.Entry<BroadBandSnmpMib, SnmpDataType>> itr = BroadBandTestConstants.WAREHOUSE_WIRELESS_SEQUENCE_OIDS_DATATYPE
		    .entrySet().iterator();
	    while (itr.hasNext()) {
		Map.Entry<BroadBandSnmpMib, SnmpDataType> entry = itr.next();
		response = BroadBandSnmpUtils.executeSnmpSetWithTableIndexOnRdkDevices(tapEnv, device,
			entry.getKey().getOid(), entry.getValue(), currentWirelessOidsValue.get(entry.getKey()),
			entry.getKey().getTableIndex());
		if (CommonMethods.isNotNull(response)
			&& response.equalsIgnoreCase(currentWirelessOidsValue.get(entry.getKey()))) {
		    count++;
		}
		tapEnv.waitTill(BroadBandTestConstants.FIVE_SECONDS_IN_MILLIS);
	    }
	    status = currentWirelessOidsValue.size() == count;
	    if (status) {
		LOGGER.info(
			"POST-CONDITION : ACTUAL : All SSIDs values are reverted to initial value successfully in Post-Condition.");
	    } else {
		LOGGER.error("POST-CONDITION : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-WH-SNMP-1007");
    }

    /**
     * Verify SNMP get operation for warehouse DOCSIS OIDs and cross-verify the same with its corresponding TR-181 param
     * using WebPA
     * <ol>
     * <li>Verify the SNMP MIB for Downstream Frequency and cross-verify using WebPA</li>
     * <li>Verify the SNMP MIB for Downstream SNR and cross-verify using WebPA</li>
     * <li>Verify the SNMP MIB for Upstream Power and cross-verify using WebPA</li>
     * <li>Verify the SNMP MIB for RF Status</li>
     * <li>Verify the SNMP MIB for Boot File and cross-verify using WebPA</li>
     * <li>Verify the SNMP MIB for RF Level and cross-verify using WebPA</li>
     * <li>Verify the SNMP MIB for Upgrade Protocol</li>
     * <li>Verify the SNMP MIB for Upgrade Status</li>
     * </ol>
     * 
     * 
     * @param device
     *            Dut instance
     * 
     * @author anandam.s
     * @refactor Rakesh C N
     * 
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WH-SNMP-1001")
    public void testToVerifyWareHouseParam(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-WH-SNMP-101";
	String stepNum = "s1";
	String errorMessage = "";
	boolean status = false;
	BroadBandResultObject result = null;
	// Variable Declation Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-WH-SNMP-1001");
	LOGGER.info(
		"TEST DESCRIPTION: Verify SNMP get operation for warehouse DOCSIS OIDs and cross-verify the same with its corresponding TR-181 param using WebPA");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Verify the SNMP MIB for Downstream Frequency and cross-verify using WebPA");
	LOGGER.info("2. Verify the SNMP MIB for Downstream SNR and cross-verify using WebPA");
	LOGGER.info("3. Verify the SNMP MIB  for Upstream Power and cross-verify using WebPA");
	LOGGER.info("4. Verify the SNMP MIB for RF Status");
	LOGGER.info("5. Verify the SNMP MIB for Boot File and cross-verify using WebPA");
	LOGGER.info("6. Verify the SNMP MIB for RF Level and cross-verify using WebPA");
	LOGGER.info("7. Verify the SNMP MIB for Upgrade Protocol");
	LOGGER.info("8. Verify the SNMP MIB for Upgrade Status");
	LOGGER.info("#######################################################################################");
	try {
	    BroadBandWebPaUtils.getPartnerIdOfDevice(device, tapEnv);
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 1: DESCRIPTION : Verify the SNMP MIB for Downstream Frequency and cross-verify using WebPA");
	    LOGGER.info(
		    "STEP 1: ACTION : Retrieve value from SNMP get using OID 1.3.6.1.2.1.10.127.1.1.1.1.2.3 and cross-verify with the value retrieved from executing webPa get on TR-181 Param Device.X_CISCO_COM_CableModem.DownstreamChannel.1.Frequency");
	    LOGGER.info(
		    "STEP 1: EXPECTED : Value retrieved from SNMP and WebPa should be the same and the value should be between 85 MHz and 860 MHz");
	    LOGGER.info("**********************************************************************************");
	    BroadBandCommonUtils utils = new BroadBandCommonUtils();
	    try {
		String webPaParam = BroadBandTestConstants.DOWNSTREAM_CHANNEL_FREQUENCY_VALUE;
		String snmpCommandOutput = BroadBandSnmpUtils.executeSnmpWalkOnRdkDevices(tapEnv, device,
			BROADBAND_WAREHOUSE_DOCSIS_SNMP_LIST.WAREHOUSE_RF_SECTION_DOWNSTREAM_FREQUENCY.getOid());
		String webpaCommandOutput = tapEnv.executeWebPaCommand(device,
			BroadBandWebPaConstants.WEBPA_PARAM_DOWNSTREAM_CHANNEL);
		result = BroadBandSnmpUtils.parseAndValidateWebpaAndSnmpResponse(
			BroadBandWebPaConstants.WEBPA_PARAM_DOWNSTREAM_CHANNEL_FREQUENCY, webpaCommandOutput,
			webPaParam, snmpCommandOutput);
		status = result.isStatus();
		errorMessage = result.getErrorMessage();
	    } catch (Exception e) {
		errorMessage = e.getMessage();
	    }
	    if (status) {
		LOGGER.info(
			"STEP 1: ACTUAL : Value retrieved from SNMP and WebPa are the same, also the value is between the expected range 85 MHz and 860 MHz");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "s2";
	    status = false;
	    result = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION : Verify the SNMP MIB for Downstream SNR and cross-verify using WebPA");
	    LOGGER.info(
		    "STEP 2: ACTION : Retrieve value from SNMP get using OID 1.3.6.1.2.1.10.127.1.1.4.1.5.3 and cross-verify with the value retrieved from executing webPa get on TR-181 Param Device.X_CISCO_COM_CableModem.DownstreamChannel.1.SNRLevel");
	    LOGGER.info(
		    "STEP 2: EXPECTED : Value retrieved from SNMP and WebPa should be the same and the value should be greater than or equal to 23.5 dB");
	    LOGGER.info("**********************************************************************************");
	    if (!DeviceModeHandler.isFibreDevice(device)) {
		result = utils.compareValueFromSnmpAndWebPa(tapEnv, device,
			BROADBAND_WAREHOUSE_DOCSIS_SNMP_LIST.WAREHOUSE_RF_SECTION_DOWNSTREAM_SNR.getOid(),
			BROADBAND_WAREHOUSE_DOCSIS_SNMP_LIST.WAREHOUSE_RF_SECTION_DOWNSTREAM_SNR.getTableIndex(),
			BroadBandWebPaConstants.WEBPA_PARAM_DOWNSTREAM_CHANNEL_SNRLEVEL);
		errorMessage = result.getErrorMessage();
		if (result.isStatus()) {
		    errorMessage = "Retrieved value is not between the expected i.e greater than or equal to 23.5 dB";
		    status = compareExpectedValues(device, result.getOutput(),
			    BroadBandWebPaConstants.WEBPA_PARAM_DOWNSTREAM_CHANNEL_SNRLEVEL);
		}
		if (status) {
		    LOGGER.info(
			    "STEP 2: ACTUAL : Value retrieved from SNMP and WebPa are the same, also the value is greater than or equal to 23.5 dB");
		} else {
		    LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    } else {
		errorMessage = "STEP 2 IS NOT APPLICABLE FOR FIBER SUPPORTED GATEWAYS(FibreDevice)";
		LOGGER.error(errorMessage);
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
			errorMessage, false);
	    }

	    stepNum = "s3";
	    result = null;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION : Verify the SNMP MIB  for Upstream Power and cross-verify using WebPA");
	    LOGGER.info(
		    "STEP 3: ACTION : Retrieve value from SNMP get using OID 1.3.6.1.2.1.10.127.1.2.2.1.3.2 and cross-verify with the value retrieved from executing webPa get on TR-181 Param Device.X_CISCO_COM_CableModem.UpstreamChannel.1.PowerLevel");
	    LOGGER.info(
		    "STEP 3: EXPECTED : Value retrieved from SNMP and WebPa should be the same and the value should be between 35 dBmV and 65 dBmV");
	    LOGGER.info("**********************************************************************************");
	    result = utils.compareValueFromSnmpAndWebPa(tapEnv, device,
		    BROADBAND_WAREHOUSE_DOCSIS_SNMP_LIST.WAREHOUSE_RF_SECTION_UPSTREAM_POWER.getOid(),
		    BROADBAND_WAREHOUSE_DOCSIS_SNMP_LIST.WAREHOUSE_RF_SECTION_UPSTREAM_POWER.getTableIndex(),
		    BroadBandWebPaConstants.WEBPA_PARAM_UPSTREAM_CHANNEL_POWERLEVEL);
	    errorMessage = result.getErrorMessage();
	    if (result.isStatus()) {
		errorMessage = "Retrieved value is not between the expected range 35 dBmV and 65 dBmV";
		status = compareExpectedValues(device, result.getOutput(),
			BroadBandWebPaConstants.WEBPA_PARAM_UPSTREAM_CHANNEL_POWERLEVEL);
	    }
	    if (status) {
		LOGGER.info(
			"STEP 3: ACTUAL : Value retrieved from SNMP and WebPa are the same, also the value is between the expected range 35 dBmV and 65 dBmV");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "s4";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 4: DESCRIPTION : Verify the SNMP MIB for RF Statusf");
	    LOGGER.info("STEP 4: ACTION : Retrieve value from OID 1.3.6.1.2.1.69.1.4.1.0 using SNMP get");
	    LOGGER.info("STEP 4: EXPECTED : Value retrieved from SNMP should be between 1 and 9");
	    LOGGER.info("**********************************************************************************");
	    String snmpOutput = BroadBandSnmpUtils.snmpGetOnEcm(tapEnv, device,
		    BROADBAND_WAREHOUSE_DOCSIS_SNMP_LIST.WAREHOUSE_RF_SECTION_RF_STATUS.getOid(),
		    BROADBAND_WAREHOUSE_DOCSIS_SNMP_LIST.WAREHOUSE_RF_SECTION_RF_STATUS.getTableIndex());
	    LOGGER.info("Value retrieved from SNMP : " + snmpOutput);
	    errorMessage = "Unable to retrieve value from SNMP using OID : "
		    + BROADBAND_WAREHOUSE_DOCSIS_SNMP_LIST.WAREHOUSE_RF_SECTION_RF_STATUS.getOid();
	    if (CommonMethods.isNotNull(snmpOutput)) {
		errorMessage = "Retrieved value is not between the expected range 1 and 9";
		status = BroadBandCommonUtils.compareValues(BroadBandTestConstants.INT_RANGE, "1-9", snmpOutput);
	    }
	    if (status) {
		LOGGER.info("STEP 4: ACTUAL : Value retrieved from SNMP is between the expected range 1 and 9");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "s5";
	    result = null;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 5: DESCRIPTION : Verify the SNMP MIB for Boot File and cross-verify using WebPA");
	    LOGGER.info(
		    "STEP 5: ACTION : Retrieve value from SNMP get using OID  1.3.6.1.2.1.69.1.4.5.0 and cross-verify with the value retrieved from executing webPa get on TR-181 Param Device.X_CISCO_COM_CableModem.DOCSISConfigFileName");
	    LOGGER.info("STEP 5: EXPECTED : Value retrieved from SNMP and WebPa should be the same");
	    LOGGER.info("**********************************************************************************");
	    result = utils.compareValueFromSnmpAndWebPa(tapEnv, device,
		    BROADBAND_WAREHOUSE_DOCSIS_SNMP_LIST.WAREHOUSE_GENERAL_BOOT_FILE.getOid(),
		    BROADBAND_WAREHOUSE_DOCSIS_SNMP_LIST.WAREHOUSE_GENERAL_BOOT_FILE.getTableIndex(),
		    BroadBandWebPaConstants.WEBPA_PARAM_BOOT_FILE);
	    errorMessage = result.getErrorMessage();
	    status = result.isStatus();
	    if (status) {
		LOGGER.info("STEP 5: ACTUAL : Value retrieved from SNMP and WebPa are the same");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "s6";
	    result = null;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 6: DESCRIPTION : Verify the SNMP MIB for RF Level and cross-verify using WebPA");
	    LOGGER.info(
		    "STEP 6: ACTION : Retrieve value from SNMP get using OID 1.3.6.1.4.1.4491.2.1.20.1.24.1.1.3 and cross-verify with the value retrieved from executing webPa get on TR-181 Param Device.X_CISCO_COM_CableModem.DownstreamChannel.1.SNRLevel");
	    LOGGER.info(
		    "STEP 6: EXPECTED : Value retrieved from SNMP and WebPa should be the same and the value should be greater than or equal to 23.5 dB");
	    LOGGER.info("**********************************************************************************");
	    if (!DeviceModeHandler.isFibreDevice(device)) {
		result = utils.compareValueFromSnmpAndWebPa(tapEnv, device,
			BROADBAND_WAREHOUSE_DOCSIS_SNMP_LIST.WAREHOUSE_RF_SECTION_LEVEL.getOid(),
			BROADBAND_WAREHOUSE_DOCSIS_SNMP_LIST.WAREHOUSE_RF_SECTION_LEVEL.getTableIndex(),
			BroadBandWebPaConstants.WEBPA_PARAM_DOWNSTREAM_CHANNEL_SNRLEVEL);
		errorMessage = result.getErrorMessage();
		if (result.isStatus()) {
		    errorMessage = "Retrieved value is not between the expected i.e greater than or equal to 23.5 dB";
		    status = compareExpectedValues(device, result.getOutput(),
			    BroadBandWebPaConstants.WEBPA_PARAM_DOWNSTREAM_CHANNEL_SNRLEVEL);
		}
		if (status) {
		    LOGGER.info(
			    "STEP 6: ACTUAL : Value retrieved from SNMP and WebPa are the same, also the value should be greater than or equal to 23.5 dB");
		} else {
		    LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    } else {
		errorMessage = "STEP 6 IS NOT APPLICABLE FOR FIBER SUPPORTED GATEWAYS(FibreDevice)";
		LOGGER.error(errorMessage);
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
			errorMessage, false);
	    }

	    stepNum = "s7";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 7: DESCRIPTION : Verify the SNMP MIB for Upgrade Protocol");
	    LOGGER.info("STEP 7: ACTION : Retrieve value from SNMP get using OID 1.3.6.1.2.1.69.1.3.8.0");
	    LOGGER.info(
		    "STEP 7: EXPECTED : Value retrieved from SNMP and WebPa should be the same and the value should be between 1 and 2");
	    snmpOutput = BroadBandSnmpUtils.snmpGetOnEcm(tapEnv, device,
		    BROADBAND_WAREHOUSE_DOCSIS_SNMP_LIST.WAREHOUSE_UPGRADE_PROTOCOL.getOid(),
		    BROADBAND_WAREHOUSE_DOCSIS_SNMP_LIST.WAREHOUSE_UPGRADE_PROTOCOL.getTableIndex());
	    LOGGER.info("Value retrieved from SNMP : " + snmpOutput);
	    errorMessage = "Unable to retrieve value from SNMP using OID : "
		    + BROADBAND_WAREHOUSE_DOCSIS_SNMP_LIST.WAREHOUSE_UPGRADE_PROTOCOL.getOid();
	    if (CommonMethods.isNotNull(snmpOutput)) {
		errorMessage = "Retrieved value is not between the expected range 1 and 2";
		status = BroadBandCommonUtils.compareValues(BroadBandTestConstants.INT_RANGE, "1-2", snmpOutput);
	    }
	    if (status) {
		LOGGER.info("STEP 7: ACTUAL : Value retrieved from SNMP is between the expected range 1 and 2");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "s8";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 8: DESCRIPTION : Verify the SNMP MIB for Upgrade Status");
	    LOGGER.info("STEP 8: ACTION : Retrieve value from SNMP get using OID 1.3.6.1.2.1.69.1.3.3.0");
	    LOGGER.info("STEP 8: EXPECTED : Value retrieved from SNMP should be between 1 and 3");
	    LOGGER.info("**********************************************************************************");
	    snmpOutput = BroadBandSnmpUtils.snmpGetOnEcm(tapEnv, device,
		    BROADBAND_WAREHOUSE_DOCSIS_SNMP_LIST.WAREHOUSE_UPGRADE_STATUS.getOid(),
		    BROADBAND_WAREHOUSE_DOCSIS_SNMP_LIST.WAREHOUSE_UPGRADE_STATUS.getTableIndex());
	    LOGGER.info("Value retrieved from SNMP : " + snmpOutput);
	    errorMessage = "Unable to retrieve value from SNMP using OID : "
		    + BROADBAND_WAREHOUSE_DOCSIS_SNMP_LIST.WAREHOUSE_UPGRADE_STATUS.getOid();
	    if (CommonMethods.isNotNull(snmpOutput)) {
		errorMessage = "Retrieved value is not between the expected range 1 and 3";
		status = BroadBandCommonUtils.compareValues(BroadBandTestConstants.INT_RANGE, "1-3", snmpOutput);
	    }
	    if (status) {
		LOGGER.info("STEP 8: ACTUAL : Value retrieved from SNMP is between the expected range 1 and 3");
	    } else {
		LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-WH-SNMP-1001");
    }

    /**
     * Utils method to compare expected values
     * 
     * @param device
     * @param output
     * @param webPaParam
     * @return
     */
    public boolean compareExpectedValues(Dut device, String output, String webPaParam) {

	boolean status = false;

	try {

	    switch (webPaParam) {
	    case BroadBandWebPaConstants.WEBPA_PARAM_DOWNSTREAM_CHANNEL_FREQUENCY:
		status = (output.length() == BroadBandTestConstants.CONSTANT_2
			|| output.length() == BroadBandTestConstants.CONSTANT_3)
				? BroadBandCommonUtils.compareValues(BroadBandTestConstants.INT_RANGE, "85-860", output)
				: BroadBandCommonUtils.compareValues(BroadBandTestConstants.BIG_INT_RANGE_INC_ZERO,
					"85000000-860000000", output);
		break;
	    case BroadBandWebPaConstants.WEBPA_PARAM_DOWNSTREAM_CHANNEL_SNRLEVEL:
		switch (tapEnv.executeWebPaCommand(device,
			BroadBandWebPaConstants.WEBPA_PARAM_DOWNSTREAM_CHANNEL_MODULATION.replace(
				BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_1))) {
		case "256 QAM":
		    status = Integer.parseInt(output) >= BroadBandTestConstants.CONSTANT_23;
		    break;
		default:
		    status = Integer.parseInt(output) >= BroadBandTestConstants.INT_VALUE_THIRTY;
		    break;
		}

		break;

	    case BroadBandWebPaConstants.WEBPA_PARAM_UPSTREAM_CHANNEL_POWERLEVEL:
		status = Integer.parseInt(output) >= BroadBandTestConstants.CONSTANT_35
			&& Integer.parseInt(output) <= BroadBandTestConstants.CONSTANT_65;
		break;

	    }

	} catch (Exception e) {
	    LOGGER.error("Exception occured while comapring values expected values : " + e.getMessage());
	}

	return status;
    }

    /**
     * Verify ECM FwUpgrade and Factory Reset OIDs - Sequence strict warehouse OIDs
     * <ol>
     * <li>Verify getting the latest GA build and current build.</li>
     * <li>Verify current firmware version of the device using SNMP.</li>
     * <li>Verify SNMP MIB docsDevSwServerTransportProtocol is set to HTTP.</li>
     * <li>Verify SNMP MIB docsDevSwServerAddressType is set to IPv6 address type.</li>
     * <li>Verify SNMP MIB docsDevSwServerAddress is set to HTTP CDL server address in Hex format</li>
     * <li>Verify SNMP MIB docsDevSwFilename is set to latest firmware filename.</li>
     * <li>Verify whether platform (RDK-B) is up and running by SNMP.</li>
     * <li>Verify SNMP MIB for firmware update and factory reset is performed with only a single reset.</li>
     * <li>Verify SNMP code download is in progress.</li>
     * <li>Verify SNMP code download completed successfully.</li>
     * <li>Verify CDL has happened successfully and new image is upgraded.</li>
     * <li>Verify setting firmware tftp ip using SNMP.</li>
     * <li>Verify SNMP MIB docsDevSwServerTransportProtocol is set to TFTP.</li>
     * <li>Verify SNMP MIB docsDevSwServerAddressType is set to IPv4 address type.</li>
     * <li>Verify SNMP MIB docsDevSwFilename is set to current device Build.</li>
     * <li>Verify SNMP MIB for firmware update and factory reset is performed with only a single reset.</li>
     * <li>Verify SNMP code download has not started.</li>
     * <li>Post-Condition 1: Verify reverting original build of the device.</li>
     * </ol>
     * 
     * @param device
     *            Dut instance
     * 
     * @author prashant mishra
     * @refactored Said Hisham
     * 
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WH-SNMP-1005")
    public void testECMFwUpgradeAndFactoryResetOIDsSequenceOnWarehouse(Dut device) {
	// Variable declaration starts
	String testCaseId = "TC-RDKB-WH-SNMP-105";
	String stepNum = "s1";
	String errorMessage = "";
	String latestImageNameToUpgrade = "";
	String currentImageName = "";
	String snmpGetResponse = "";
	String snmpSetResponse = "";
	boolean status = false;
	// Variable declaration ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-WH-SNMP-1005");
	LOGGER.info("TEST DESCRIPTION: Verify ECM FwUpgrade and Factory Reset OIDs - Sequence strict warehouse OIDs");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Verify getting the latest GA build and current build.");
	LOGGER.info("2. Verify current firmware version of the device using SNMP.");
	LOGGER.info("3. Verify SNMP MIB docsDevSwServerTransportProtocol is set to HTTP.");
	LOGGER.info("4. Verify SNMP MIB docsDevSwServerAddressType is set to IPv6 address type.");
	LOGGER.info("5. Verify SNMP MIB docsDevSwServerAddress is set to HTTP CDL server address in Hex format");
	LOGGER.info("6. Verify SNMP MIB docsDevSwFilename is set to latest firmware filename.");
	LOGGER.info("7. Verify whether platform (RDK-B) is up and running by SNMP.");
	LOGGER.info("8. Verify SNMP MIB for firmware update and factory reset is performed with only a single reset.");
	LOGGER.info("9. Verify SNMP code download is in progress.");
	LOGGER.info("10. Verify SNMP code download completed successfully.");
	LOGGER.info("11. Verify CDL has happened successfully and new image is upgraded.");
	LOGGER.info("12. Verify setting firmware tftp ip using SNMP.");
	LOGGER.info("13. Verify SNMP MIB docsDevSwServerTransportProtocol is set to TFTP.");
	LOGGER.info("14. Verify SNMP MIB docsDevSwServerAddressType is set to IPv4 address type.");
	LOGGER.info("15. Verify SNMP MIB docsDevSwFilename is set to current device Build.");
	LOGGER.info("16. Verify SNMP MIB for firmware update and factory reset is performed with only a single reset.");
	LOGGER.info("17. Verify SNMP code download has not started.");
	LOGGER.info("POST-CONDITION 1: Verify reverting original build of the device.");

	LOGGER.info("#######################################################################################");

	try {
	    errorMessage = "Both latest GA and current build are same so ending test case here.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Verify getting the latest GA build and current build.");
	    LOGGER.info("STEP 1: ACTION : Get latest GA and current build.");
	    LOGGER.info("STEP 1: EXPECTED : Both latest GA and current build should not be same and null.");
	    LOGGER.info("**********************************************************************************");
	    if (BroadBandXconfCdlUtils.toClearCdlInfoInXconf(device, tapEnv)) {
		LOGGER.info("Cleared cdl info in xconf");
	    }
	    currentImageName = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);
	    LOGGER.info("CURRENT IMAGE OF THE DEVICE: " + currentImageName);

	    latestImageNameToUpgrade = tapEnv.getLatestBuildImageVersionForCdlTrigger(device, false);
	    LOGGER.info("LATEST FIRMWARE VERSION: " + latestImageNameToUpgrade);
	    if (CommonMethods.isNull(latestImageNameToUpgrade)) {
		LOGGER.info(
			" GA image obtained from deployed version service is null. Hence getting the image from property file ");
		latestImageNameToUpgrade = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(
			device, BroadBandPropertyKeyConstants.PARTIAL_PROPERTY_KEY_FOR_GA_IMAGE);
		LOGGER.info("Latest Firmware version from property file: " + latestImageNameToUpgrade);
	    }

	    LOGGER.info("LATEST IMAGE TO UPGRADE: " + latestImageNameToUpgrade);
	    status = CommonMethods.isNotNull(currentImageName) && CommonMethods.isNotNull(latestImageNameToUpgrade)
		    && !latestImageNameToUpgrade.equals(currentImageName);
	    errorMessage = latestImageNameToUpgrade.equals(currentImageName)
		    ? "Device is already having latest Image file."
		    : "Unable to get latest and current image name.";
	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : Latest and current Image name obtained successfully.");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "s2";
	    errorMessage = "Unable to verify current firmware using SNMP Get command.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION : Verify current firmware version of the device using SNMP.");
	    LOGGER.info("STEP 2: ACTION : Execute the SNMP Get command for following Mib: 1.3.6.1.2.1.69.1.3.5.0");
	    LOGGER.info("STEP 2: EXPECTED : SNMP Get command should return current firmware version.");
	    LOGGER.info("**********************************************************************************");
	    snmpGetResponse = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
		    BroadBandSnmpMib.ECM_DOCS_DEV_CURRENT_SOFTWARE_VERSION_WITHOUT_INDEX.getOid(),
		    BroadBandTestConstants.STRING_ZERO);
	    status = CommonMethods.isNotNull(snmpGetResponse)
		    && !snmpGetResponse.contains(BroadBandSnmpConstants.SNMP_ERROR_RESPONSE_NO_OBJECT)
		    && !snmpGetResponse.contains(BroadBandSnmpConstants.SNMP_ERROR_RESPONSE_NO_OID)
		    && !snmpGetResponse.toLowerCase().contains(BroadBandTestConstants.STRING_TIMEOUT);
	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : SNMP MIB docsDevSwCurrentVers verified successfully.");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    /**
	     * Method to set Download Protocol, Server Address Type, Server address, Firmware image for HTTP Ipv6
	     * download
	     */
	    methodToSetParamsForSnmpCodeDownloadHttpIpv6(tapEnv, device, latestImageNameToUpgrade, testCaseId,
		    BroadBandTestConstants.CONSTANT_3);

	    stepNum = "s7";
	    errorMessage = "RDK-B is not running.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 7: DESCRIPTION : Verify whether platform (RDK-B) is up and running by SNMP.");
	    LOGGER.info("STEP 7: ACTION : Execute SNMP GET command with  oid as 1.3.6.1.4.1.17270.50.2.1.1.1006.0");
	    LOGGER.info("STEP 7: EXPECTED : SNMP Get output should be 1.");
	    LOGGER.info("**********************************************************************************");
	    snmpGetResponse = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
		    BroadBandSnmpMib.ESTB_RDKB_RUNNING.getOid(), BroadBandTestConstants.STRING_ZERO);
	    status = BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
		    BroadBandTestConstants.STRING_VALUE_ONE, snmpGetResponse);
	    if (status) {
		LOGGER.info("STEP 7: ACTUAL : RDK-B is running.");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "s8";
	    errorMessage = "snmpset on SNMP MIB firmwareupdate(1.3.6.1.4.1.17270.50.2.1.1.1008.0) as 1 failed.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 8: DESCRIPTION : Verify SNMP MIB for firmware update and factory reset is performed with only a single reset.");
	    LOGGER.info(
		    "STEP 8: ACTION : Execute SNMP SET command with  oid as 1.3.6.1.4.1.17270.50.2.1.1.1008.0 and value to be set as 1.");
	    LOGGER.info(
		    "STEP 8: EXPECTED : SNMP MIB firmwareupdate and factory reset should upgrade the image and does factory reset.");
	    LOGGER.info("**********************************************************************************");
	    int count = 1;
	    if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
		BroadBandCommonUtils.getAtomDeviceUptimeStatus(device, tapEnv);
	    }
	    while (count <= 7) {
		snmpSetResponse = BroadBandSnmpUtils.executeSnmpSetWithTableIndexOnRdkDevices(tapEnv, device,
			BroadBandSnmpMib.ECM_FWUPGRADE_AND_FACTORYRESET_WITHOUT_INDEX.getOid(), SnmpDataType.INTEGER,
			BroadBandTestConstants.STRING_VALUE_ONE, BroadBandTestConstants.STRING_ZERO);
		status = BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
			BroadBandTestConstants.STRING_VALUE_ONE, snmpSetResponse);
		if (status) {
		    break;
		}
		tapEnv.waitTill(BroadBandTestConstants.THREE_MINUTE_IN_MILLIS);
		count++;
	    }
	    if (status) {
		LOGGER.info("STEP 8: ACTUAL : SNMP MIB firmwareupdate and factory reset is set to 1 successfully.");
	    } else {
		LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "s9";
	    errorMessage = "SNMP code download has not started.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 9: DESCRIPTION : Verify SNMP code download is in progress.");
	    LOGGER.info("STEP 9: ACTION : Execute SNMP GET command with  oid as 1.3.6.1.2.1.69.1.3.4.0.");
	    LOGGER.info("STEP 9: EXPECTED : SNMP Get should return the Integer value 1 or 5.");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandCodeDownloadUtils.isUpgradeStatusInProgressUsingSnmpCommand(tapEnv, device);
	    if (status) {
		LOGGER.info("STEP 9: ACTUAL : SNMP code download via HTTP(Ipv6) server started successfully.");
	    } else {
		LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "s10";
	    errorMessage = "SNMP code download has not completed successfully.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 10: DESCRIPTION : Verify SNMP code download completed successfully.");
	    LOGGER.info("STEP 10: ACTION : Execute SNMP GET command with  oid as 1.3.6.1.2.1.69.1.3.4.0.");
	    LOGGER.info("STEP 10: EXPECTED : SNMP Get should return the Integer value 3 or 5.");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandCodeDownloadUtils.verifySnmpCodeDownlaodCompletionStatus(tapEnv, device);
	    if (status) {
		LOGGER.info("STEP 10: ACTUAL : SNMP code download completed successfully.");
	    } else {
		LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "s11";
	    errorMessage = "Image file name obtained in step 1 and image file name of device should be same.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 11: DESCRIPTION : Verify CDL has happened successfully and new image is upgraded.");
	    LOGGER.info("STEP 11: ACTION : Execute SNMP GET command with  oid as 1.3.6.1.2.1.69.1.3.5");
	    LOGGER.info("STEP 11: EXPECTED : SNMP Get should return the image file name of the device.");
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("Waiting for 1 minute to verify upgraded image name after successful download.");
	    tapEnv.waitTill(BroadBandTestConstants.TEN_MINUTE_IN_MILLIS);

	    if (BroadBandCommonUtils.isSTBAccessible(tapEnv, device, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, 10)) {
		status = BroadBandCodeDownloadUtils.isImageUpgradedInDevice(tapEnv, device, latestImageNameToUpgrade);
	    }
	    if (status) {
		LOGGER.info(
			"STEP 11: ACTUAL : Device build verified to latest build successfully after SNMP HTTP(IPv6) upgrade.");
	    } else {
		LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    /**
	     * Method to set Download Protocol, Server Address Type, Server address, Firmware image for TFTP Ipv4
	     * download
	     */
	    methodToSetParamsForSnmpCodeDownloadTFTPIpv4(tapEnv, device, latestImageNameToUpgrade, testCaseId,
		    BroadBandTestConstants.CONSTANT_12);

	    stepNum = "s16";
	    errorMessage = "snmpset on SNMP MIB firmwareupdate(1.3.6.1.4.1.17270.50.2.1.1.1008.0) as 1 failed.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 16: DESCRIPTION : Verify SNMP MIB for firmware update and factory reset is performed with only a single reset.");
	    LOGGER.info(
		    "STEP 16: ACTION : Execute SNMP SET command with  oid as 1.3.6.1.4.1.17270.50.2.1.1.1008.0 and value to be set as 1.");
	    LOGGER.info(
		    "STEP 16: EXPECTED : SNMP MIB firmwareupdate and factory reset should upgrade the image and does factory reset.");
	    LOGGER.info("**********************************************************************************");
	    count = 1;
	    if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
		BroadBandCommonUtils.getAtomSyncUptimeStatus(device, tapEnv);
	    }
	    while (count <= 7) {
		snmpSetResponse = BroadBandSnmpUtils.executeSnmpSetWithTableIndexOnRdkDevices(tapEnv, device,
			BroadBandSnmpMib.ECM_FWUPGRADE_AND_FACTORYRESET_WITHOUT_INDEX.getOid(), SnmpDataType.INTEGER,
			BroadBandTestConstants.STRING_VALUE_ONE, BroadBandTestConstants.STRING_ZERO);
		status = BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
			BroadBandTestConstants.STRING_VALUE_ONE, snmpSetResponse);
		if (status) {
		    break;
		}
		tapEnv.waitTill(BroadBandTestConstants.THREE_MINUTE_IN_MILLIS);
		count++;
	    }
	    if (status) {
		LOGGER.info("STEP 16: ACTUAL : SNMP MIB firmwareupdate and factory reset is set to 1 successfully.");
	    } else {
		LOGGER.error("STEP 16: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "s17";
	    errorMessage = "SNMP code download started even after giving device build as current build.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 17: DESCRIPTION : Verify SNMP code download has not started.");
	    LOGGER.info("STEP 17: ACTION : Execute SNMP GET command with  oid as 1.3.6.1.2.1.69.1.3.4.0.");
	    LOGGER.info("STEP 17: EXPECTED : SNMP Get should return the Integer value 4 or 5.");
	    LOGGER.info("**********************************************************************************");
	    status = !BroadBandCodeDownloadUtils.isUpgradeStatusInProgressUsingSnmpCommand(tapEnv, device);
	    if (!status) {
		LOGGER.info("Code download started. Will verify for code download failed.");
		status = !BroadBandCodeDownloadUtils.verifySnmpCodeDownlaodCompletionStatus(tapEnv, device);
	    }
	    if (status) {
		LOGGER.info(
			"STEP 17: ACTUAL : SNMP code download via TFTP(Ipv4) server has not started as build to be upgrade is given as device current build.");
	    } else {
		LOGGER.error("STEP 17: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	} catch (Exception exception) {
	    errorMessage = errorMessage + exception.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    int postConditionStepNum = 0;

	    status = false;
	    String imageAfterTriggering = "";
	    errorMessage = "Device is not accessible even after waiting for 10 mins.";
	    String successMessage = "";
	    postConditionStepNum++;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("POST-CONDITION " + postConditionStepNum
		    + ": DESCRIPTION : Verify reverting device to original image.");
	    LOGGER.info("POST-CONDITION " + postConditionStepNum
		    + ": ACTION : Flash the original build on the device using HTTP/ TR-181.");
	    LOGGER.info("POST-CONDITION " + postConditionStepNum
		    + ": EXPECTED : Device should be upgraded to original image.");
	    LOGGER.info("**********************************************************************************");
	    try {
		if (BroadBandCommonUtils.isSTBAccessible(tapEnv, device, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
			BroadBandTestConstants.CONSTANT_10)) {
		    errorMessage = "Webap is not up and running.";
		    if (BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true)) {
			errorMessage = "Unable to get current firmware version.";
			imageAfterTriggering = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);
			if (CommonMethods.isNotNull(imageAfterTriggering)
				&& CommonMethods.isNotNull(currentImageName)) {
			    if (!imageAfterTriggering.equals(currentImageName)) {
				status = BroadBandCodeDownloadUtils.upgradeDeviceWithGivenFirmwareVersion(device,
					tapEnv, currentImageName);
				if (!status) {
				    status = BroadBandCodeDownloadUtils.triggerPreviousCodeDownload(device, tapEnv,
					    currentImageName);
				}
			    } else {
				successMessage = "Device Build hasn't changed so need to revert the device image.";
				status = true;
			    }
			}
		    }
		}
	    } catch (Exception e) {
		errorMessage = "Exception occured during reverting the device back to original image." + errorMessage
			+ e.getMessage();
	    }
	    if (status) {
		LOGGER.info("POST-CONDITION " + postConditionStepNum + ": ACTUAL : " + successMessage);
	    } else {
		LOGGER.error("POST-CONDITION " + postConditionStepNum + ": ACTUAL : " + errorMessage);
	    }
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-WH-SNMP-1005");
    }

    /**
     * Common Method to set Param for SNMP code download via HTTP(IPv6)
     * 
     * @param tapEnv
     *            AutomaticsTapApi device
     * @param device
     *            Dut device
     * @param firmware
     *            Firmware to be upgraded
     * @param stepNumber
     *            Starting Step Number
     * @param testCaseId
     *            Test case id
     */
    public static void methodToSetParamsForSnmpCodeDownloadHttpIpv6(AutomaticsTapApi tapEnv, Dut device,
	    String firmware, String testCaseId, int stepNumber) {
	// Variable declaration starts
	String errorMessage = "";
	boolean status = false;
	BroadBandResultObject result = null;
	String stepNum = "";
	// Variable declaration ends
	stepNum = "s" + stepNumber;
	errorMessage = "snmpset on docsDevSwServerTransportProtocol(1.3.6.1.2.1.69.1.3.8.0) failed.";
	status = false;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP " + stepNumber
		+ " : DESCRIPTION : Verify SNMP MIB docsDevSwServerTransportProtocol is set to HTTP.");
	LOGGER.info("STEP " + stepNumber
		+ " : ACTION : Execute SNMP SET command with  oid as 1.3.6.1.2.1.69.1.3.8.0 and value to be set as  2  and SNMP get to verify same set value.");
	LOGGER.info("STEP " + stepNumber
		+ " : EXPECTED : SNMP MIB  docsDevSwServerTransportProtocol should be set to HTTP(2) and SNMP Get output should be same as set value.");
	LOGGER.info("**********************************************************************************");
	result = BroadBandSnmpUtils.snmpSetAndVerifySNMPResponseRdkDevices(device, tapEnv,
		BroadBandSnmpMib.ECM_SERVER_TRANSPORT_PROTOCOL_WITHOUT_INDEX.getOid(),
		BroadBandTestConstants.STRING_VALUE_TWO, SnmpDataType.INTEGER,
		BroadBandTestConstants.STRING_VALUE_ZERO);
	status = result.isStatus();
	errorMessage = errorMessage + result.getErrorMessage();
	if (status) {
	    LOGGER.info(
		    "STEP " + stepNumber + " : ACTUAL : SNMP MIB docsDevSwServerTransportProtocol is set to HTTP(2).");
	} else {
	    LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	stepNumber++;
	stepNum = "s" + stepNumber;
	errorMessage = "snmpset on docsDevSwServerAddressType(OID) failed.";
	status = false;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP " + stepNumber
		+ " : DESCRIPTION : Verify SNMP MIB docsDevSwServerAddressType is set to IPv6 address type.");
	LOGGER.info("STEP " + stepNumber
		+ " : ACTION : Execute SNMP SET command with to be set docsDevSwServerAddressType value as  2  and SNMP get to verify same set value.");
	LOGGER.info("STEP " + stepNumber
		+ " : EXPECTED : SNMP MIB  docsDevSwServerAddressType should be set to IPv6 address type(2)  and SNMP Get output should be same as set value.");
	LOGGER.info("**********************************************************************************");
	result = BroadBandSnmpUtils.snmpSetAndVerifySNMPResponseRdkDevices(device, tapEnv,
		BroadBandSnmpMib.ECM_SERVER_ADDRESS_TYPE_CDL_WITHOUT_INDEX.getOid(),
		BroadBandTestConstants.STRING_VALUE_TWO, SnmpDataType.INTEGER,
		BroadBandTestConstants.STRING_VALUE_ZERO);
	status = result.isStatus();
	errorMessage = errorMessage + result.getErrorMessage();
	if (status) {
	    LOGGER.info("STEP " + stepNumber
		    + " : ACTUAL : SNMP MIB docsDevSwServerAddressType is set to IPv6 address type(2).");
	} else {
	    LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	stepNumber++;
	stepNum = "s" + stepNumber;
	errorMessage = "snmpset on docsDevSwServerAddress(1.3.6.1.2.1.69.1.3.7.0) with server address protocol failed.";
	status = false;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP " + stepNumber
		+ " : DESCRIPTION : Verify SNMP MIB docsDevSwServerAddress is set to HTTP CDL server address in Hex format.");
	LOGGER.info("STEP " + stepNumber
		+ " : ACTION : Execute SNMP SET command with  oid as 1.3.6.1.2.1.69.1.3.7.0 and value to be set as server address and SNMP Get to verify same set value.");
	LOGGER.info("STEP " + stepNumber
		+ " : EXPECTED : SNMP MIB  docsDevSwServerAddressType should be set to HTTP CDL server address in Hex format  and SNMP Get output should be same as set value.");
	LOGGER.info("**********************************************************************************");
	status = FirmwareDownloadUtils.setServerAddressForSnmpCodeDownload(tapEnv, device);
	if (status) {
	    LOGGER.info("STEP " + stepNumber
		    + " : ACTUAL : SNMP MIB docsDevSwServerAddres is set to HTTP CDL server address in Hex format.");
	} else {
	    LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	stepNumber++;
	stepNum = "s" + stepNumber;
	errorMessage = "snmpset on docsDevSwFilename(1.3.6.1.2.1.69.1.3.2.0) with latest image version failed.";
	status = false;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP " + stepNumber
		+ " : DESCRIPTION : Verify SNMP MIB docsDevSwFilename is set to latest firmware filename.");
	LOGGER.info("STEP " + stepNumber
		+ " : ACTION : Execute SNMP SET command with  oid as 1.3.6.1.2.1.69.1.3.2.0 and value to be set as Image file name obtained in step 1  and SNMP get to verify same set value.");
	LOGGER.info("STEP " + stepNumber
		+ " : EXPECTED : SNMP MIB  docsDevSwFilename should be set to latest firmware  and SNMP Get output should be same as set value.");
	LOGGER.info("**********************************************************************************");
	result = BroadBandSnmpUtils.snmpSetAndVerifySNMPResponseRdkDevices(device, tapEnv,
		BroadBandSnmpMib.ECM_DOCS_DEV_SW_FILE_NAME_WITHOUT_INDEX.getOid(),
		firmware + BroadBandCdlConstants.BIN_EXTENSION, SnmpDataType.STRING,
		BroadBandTestConstants.STRING_VALUE_ZERO);
	status = result.isStatus();
	errorMessage = errorMessage + result.getErrorMessage();
	if (status) {
	    LOGGER.info("STEP " + stepNumber + " : ACTUAL : SNMP MIB docsDevSwFilename is set to latest firmware.");
	} else {
	    LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
    }

    /**
     * Common Method to set Param for SNMP code download via TFTP(IPv4)
     * 
     * @param tapEnv
     *            AutomaticsTapApi instance
     * @param device
     *            Dut instance
     * @param firmware
     *            Firmware to be upgraded
     * @param testCaseId
     *            Test case id
     * @param stepNumber
     *            Starting step Number
     */
    public static void methodToSetParamsForSnmpCodeDownloadTFTPIpv4(AutomaticsTapApi tapEnv, Dut device,
	    String firmware, String testCaseId, int stepNumber) {
	// Variable declaration starts
	String errorMessage = "";
	boolean status = false;
	BroadBandResultObject result = null;
	String stepNum = "";
	// Variable declaration ends

	stepNum = "s" + stepNumber;
	errorMessage = "Unable to set firmware tftp ip using SNMP Set Command.";
	status = false;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : Verify setting firmware tftp ip using SNMP.");
	LOGGER.info("STEP " + stepNumber
		+ " : ACTION : Execute the SNMP Set command for following Mib: 1.3.6.1.2.1.69.1.3.1.0 & set value as \"96.114.220.246\" and SNMP get to verify same set value.");
	LOGGER.info("STEP " + stepNumber
		+ " : EXPECTED : SNMP Set Command should be executed successfully and SNMP Get output should be same as set value.");
	LOGGER.info("**********************************************************************************");
	result = BroadBandSnmpUtils.snmpSetAndVerifySNMPResponseRdkDevices(device, tapEnv,
		BroadBandSnmpMib.ECM_DOCS_DEV_SW_IPV4_SERVER_ADDRESS.getOid(),
		AutomaticsTapApi.getSTBPropsValue(BroadBandPropertyKeyConstants.PROP_KEY_TFTP_SERVER_IP_ADDRESS),
		SnmpDataType.STRING_A, BroadBandTestConstants.STRING_VALUE_ZERO);
	status = result.isStatus();
	errorMessage = errorMessage + result.getErrorMessage();
	if (status) {
	    LOGGER.info("STEP " + stepNumber
		    + " : ACTUAL : SNMP MIB docsDevSwServerAddres is set to TFTP CDL IPv4 server address.");
	} else {
	    LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	stepNumber++;
	stepNum = "s" + stepNumber;
	errorMessage = "snmpset on docsDevSwServerTransportProtocol(1.3.6.1.2.1.69.1.3.8.0) failed.";
	status = false;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP " + stepNumber
		+ " : DESCRIPTION : Verify SNMP MIB docsDevSwServerTransportProtocol is set to TFTP.");
	LOGGER.info("STEP " + stepNumber
		+ " : ACTION : Execute SNMP SET command with  oid as 1.3.6.1.2.1.69.1.3.8.0 and value to be set as 1 and SNMP get to verify same set value.");
	LOGGER.info("STEP " + stepNumber
		+ " : EXPECTED : SNMP MIB  docsDevSwServerTransportProtocol should be set to TFTP(1) and SNMP Get output should be same as set value.");
	LOGGER.info("**********************************************************************************");
	result = BroadBandSnmpUtils.snmpSetAndVerifySNMPResponseRdkDevices(device, tapEnv,
		BroadBandSnmpMib.ECM_SERVER_TRANSPORT_PROTOCOL_WITHOUT_INDEX.getOid(),
		BroadBandTestConstants.STRING_VALUE_ONE, SnmpDataType.INTEGER,
		BroadBandTestConstants.STRING_VALUE_ZERO);
	status = result.isStatus();
	errorMessage = errorMessage + result.getErrorMessage();
	if (status) {
	    LOGGER.info(
		    "STEP " + stepNumber + " : ACTUAL : SNMP MIB docsDevSwServerTransportProtocol is set to TFTP(1).");
	} else {
	    LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	stepNumber++;
	stepNum = "s" + stepNumber;
	errorMessage = "snmpset on docsDevSwServerAddressType(OID) failed.";
	status = false;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP " + stepNumber
		+ " : DESCRIPTION : Verify SNMP MIB docsDevSwServerAddressType is set to IPv4 address type.");
	LOGGER.info("STEP " + stepNumber
		+ " : ACTION : Execute SNMP SET command to set docsDevSwServerAddressType and value to be set as  1  and SNMP get to verify same set value.");
	LOGGER.info("STEP " + stepNumber
		+ " : EXPECTED : SNMP MIB  docsDevSwServerAddressType should be set to IPv4 address type(1) and SNMP Get output should be same as set value.");
	LOGGER.info("**********************************************************************************");
	result = BroadBandSnmpUtils.snmpSetAndVerifySNMPResponseRdkDevices(device, tapEnv,
		BroadBandSnmpMib.ECM_SERVER_ADDRESS_TYPE_CDL_WITHOUT_INDEX.getOid(),
		BroadBandTestConstants.STRING_VALUE_ONE, SnmpDataType.INTEGER,
		BroadBandTestConstants.STRING_VALUE_ZERO);
	status = result.isStatus();
	errorMessage = errorMessage + result.getErrorMessage();
	if (status) {
	    LOGGER.info("STEP " + stepNumber
		    + " : ACTUAL : SNMP MIB docsDevSwServerAddressType is set to IPv4 address type(1).");
	} else {
	    LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	stepNumber++;
	stepNum = "s" + stepNumber;
	errorMessage = "snmpset on docsDevSwFilename(1.3.6.1.2.1.69.1.3.2.0) with current device build got  failed.";
	status = false;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP " + stepNumber
		+ " : DESCRIPTION : Verify SNMP MIB docsDevSwFilename is set to current device Build.");
	LOGGER.info("STEP " + stepNumber
		+ " : ACTION : Execute SNMP SET command with  oid as 1.3.6.1.2.1.69.1.3.2.0 and value to be set as current Build of the device  and SNMP get to verify same set value.");
	LOGGER.info("STEP " + stepNumber
		+ " : EXPECTED : SNMP MIB  docsDevSwFilename should be set to latest firmware  and SNMP Get output should be same as set value.");
	LOGGER.info("**********************************************************************************");
	result = BroadBandSnmpUtils.snmpSetAndVerifySNMPResponseRdkDevices(device, tapEnv,
		BroadBandSnmpMib.ECM_DOCS_DEV_SW_FILE_NAME_WITHOUT_INDEX.getOid(),
		firmware + BroadBandCdlConstants.BIN_EXTENSION, SnmpDataType.STRING,
		BroadBandTestConstants.STRING_VALUE_ZERO);
	status = result.isStatus();
	errorMessage = errorMessage + result.getErrorMessage();
	if (status) {
	    LOGGER.info("STEP " + stepNumber
		    + " : ACTUAL : SNMP MIB docsDevSwFilename is set to current Build of the device.");
	} else {
	    LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
    }

    /**
     * Verify ECM Docs DevSw Admin Status OIDs - Sequence strict warehouse OIDs
     * <ol>
     * <li>Verify getting the latest GA build and current build.</li>
     * <li>Verify SNMP MIB docsDevSwServerTransportProtocol is set to HTTP.</li>
     * <li>Verify SNMP MIB docsDevSwServerAddressType is set to IPv6 address type.</li>
     * <li>Verify SNMP MIB docsDevSwServerAddress is set to HTTP CDL server address in Hex format</li>
     * <li>Verify SNMP MIB docsDevSwFilename is set to latest firmware filename.</li>
     * <li>Verify SNMP MIB docsDevSwAdminStatus is set for starting the download.</li>
     * <li>Verify SNMP code download is in progress.</li>
     * <li>Verify SNMP code download completed successfully.</li>
     * <li>Verify CDL has happened successfully and new image is upgraded.</li>
     * <li>Verify setting firmware tftp ip using SNMP.</li>
     * <li>Verify SNMP MIB docsDevSwServerTransportProtocol is set to TFTP.</li>
     * <li>Verify SNMP MIB docsDevSwServerAddressType is set to IPv4 address type.</li>
     * <li>Verify SNMP MIB docsDevSwFilename is set to current device Build.</li>
     * <li>Verify SNMP MIB docsDevSwAdminStatus is set for starting the download.</li>
     * <li>Verify SNMP code download has not started.</li>
     * <li>Verify SNMP MIB FACTORY_RESET_DEVICE is set for Factory Resetting the device.</li>
     * <li>Post-Condition 1: Verify reverting original build of the device.</li>
     * </ol>
     * 
     * @param device
     *            Dut instance
     * 
     * @author prashant mishra
     * @refactor yamini.s
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WH-SNMP-1006")
    public void testECMDocsDevSwAdminStatusOIDsSequenceOnWarehouse(Dut device) {
	// Variable declaration starts
	String testCaseId = "TC-RDKB-WH-SNMP-106";
	String stepNum = "s1";
	String errorMessage = "";
	String latestImageNameToUpgrade = "";
	String currentImageName = "";
	String snmpSetResponse = "";
	boolean status = false;
	// Variable declaration ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-WH-SNMP-1006");
	LOGGER.info("TEST DESCRIPTION: Verify ECM Docs DevSw Admin Status OIDs - Sequence strict warehouse OIDs");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Verify getting the latest GA build and current build.");
	LOGGER.info("3. Verify SNMP MIB docsDevSwServerTransportProtocol is set to HTTP.");
	LOGGER.info("3. Verify SNMP MIB docsDevSwServerAddressType is set to IPv6 address type.");
	LOGGER.info("4. Verify SNMP MIB docsDevSwServerAddress is set to HTTP CDL server address in Hex format");
	LOGGER.info("5. Verify SNMP MIB docsDevSwFilename is set to latest firmware filename.");
	LOGGER.info("6. Verify SNMP MIB docsDevSwAdminStatus is set for starting the download.");
	LOGGER.info("7. Verify SNMP code download is in progress.");
	LOGGER.info("8. Verify SNMP code download completed successfully.");
	LOGGER.info("9. Verify CDL has happened successfully and new image is upgraded.");
	LOGGER.info("10. Verify setting firmware tftp ip using SNMP.");
	LOGGER.info("11. Verify SNMP MIB docsDevSwServerTransportProtocol is set to TFTP.");
	LOGGER.info("12. Verify SNMP MIB docsDevSwServerAddressType is set to IPv4 address type.");
	LOGGER.info("13. Verify SNMP MIB docsDevSwFilename is set to current device Build.");
	LOGGER.info("14. Verify SNMP MIB docsDevSwAdminStatus is set for starting the download.");
	LOGGER.info("15. Verify SNMP code download has not started.");
	LOGGER.info("16. Verify SNMP MIB FACTORY_RESET_DEVICE is set for Factory Resetting the device.");
	LOGGER.info("POST-CONDITION 1: Verify reverting original build of the device.");

	LOGGER.info("#######################################################################################");

	try {
	    errorMessage = "Both latest GA and current build are same so ending test case here.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Verify getting the latest GA build and current build.");
	    LOGGER.info("STEP 1: ACTION : Get latest GA and current build.");
	    LOGGER.info("STEP 1: EXPECTED : Both latest GA and current build should not be same and null.");
	    LOGGER.info("**********************************************************************************");
	    if (BroadBandXconfCdlUtils.toClearCdlInfoInXconf(device, tapEnv)) {
		LOGGER.info("Cleared cdl info in xconf");
	    }
	    currentImageName = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);
	    LOGGER.info("CURRENT IMAGE OF THE DEVICE: " + currentImageName);

	    latestImageNameToUpgrade = tapEnv.getLatestBuildImageVersionForCdlTrigger(device, false);
	    LOGGER.info("LATEST FIRMWARE VERSION: " + latestImageNameToUpgrade);
	    if (CommonMethods.isNull(latestImageNameToUpgrade)) {
		LOGGER.info(
			" GA image obtained from deployed version service is null. Hence getting the image from property file ");
		latestImageNameToUpgrade = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(
			device, BroadBandPropertyKeyConstants.PARTIAL_PROPERTY_KEY_FOR_GA_IMAGE);
		LOGGER.info("Latest Firmware version from property file: " + latestImageNameToUpgrade);
	    }
	    LOGGER.info("LATEST IMAGE TO UPGRADE: " + latestImageNameToUpgrade);
	    status = CommonMethods.isNotNull(currentImageName) && CommonMethods.isNotNull(latestImageNameToUpgrade)
		    && !latestImageNameToUpgrade.equals(currentImageName);
	    errorMessage = latestImageNameToUpgrade.equals(currentImageName)
		    ? "Device is already having latest Image file."
		    : "Unable to get latest and current image name.";
	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : Latest and current Image name obtained successfully.");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    /**
	     * Method to set Download Protocol, Server Address Type, Server address, Firmware image for HTTP Ipv6
	     * download
	     */
	    methodToSetParamsForSnmpCodeDownloadHttpIpv6(tapEnv, device, latestImageNameToUpgrade, testCaseId,
		    BroadBandTestConstants.CONSTANT_2);

	    stepNum = "s6";
	    errorMessage = "snmpset on SNMP MIB docsDevSwAdminStatus(1.3.6.1.2.1.69.1.3.3.0) as 1 failed.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 6: DESCRIPTION : Verify SNMP MIB docsDevSwAdminStatus is set for starting the download.");
	    LOGGER.info(
		    "STEP 6: ACTION : Execute SNMP SET command with  oid as 1.3.6.1.2.1.69.1.3.3.0 and value to be set as 1.");
	    LOGGER.info("STEP 6: EXPECTED : SNMP MIB 1.3.6.1.2.1.69.1.3.3.0 should be set to 1.");
	    LOGGER.info("**********************************************************************************");
	    int count = 1;
	    while (count <= 7) {
		snmpSetResponse = BroadBandSnmpUtils.executeSnmpSetWithTableIndexOnRdkDevices(tapEnv, device,
			BroadBandSnmpMib.ECM_DOCS_DEVSW_ADMIN_STATAUS_WITHOUT_INDEX.getOid(), SnmpDataType.INTEGER,
			BroadBandTestConstants.STRING_VALUE_ONE, BroadBandTestConstants.STRING_ZERO);
		status = BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
			BroadBandTestConstants.STRING_VALUE_ONE, snmpSetResponse);
		if (status) {
		    break;
		}
		tapEnv.waitTill(BroadBandTestConstants.THREE_MINUTE_IN_MILLIS);
		count++;
	    }
	    if (status) {
		LOGGER.info("STEP 6: ACTUAL : SNMP MIB docsDevSwAdminStatus is set to 1 successfully.");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "s7";
	    errorMessage = "SNMP code download has not started.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 7: DESCRIPTION : Verify SNMP code download is in progress.");
	    LOGGER.info("STEP 7: ACTION : Execute SNMP GET command with  oid as 1.3.6.1.2.1.69.1.3.4.0.");
	    LOGGER.info("STEP 7: EXPECTED : SNMP Get should return the Integer value 1.");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandCodeDownloadUtils.isUpgradeStatusInProgressUsingSnmpCommand(tapEnv, device);
	    if (status) {
		LOGGER.info("STEP 7: ACTUAL : SNMP code download via HTTP(Ipv6) server started successfully.");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "s8";
	    errorMessage = "SNMP code download has not completed successfully.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 8: DESCRIPTION : Verify SNMP code download completed successfully.");
	    LOGGER.info("STEP 8: ACTION : Execute SNMP GET command with  oid as 1.3.6.1.2.1.69.1.3.4.0.");
	    LOGGER.info("STEP 8: EXPECTED : SNMP Get should return the Integer value 3 or 5.");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandCodeDownloadUtils.verifySnmpCodeDownlaodCompletionStatus(tapEnv, device);
	    if (status) {
		LOGGER.info("STEP 8: ACTUAL : SNMP code download completed successfully.");
	    } else {
		LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "s9";
	    errorMessage = "Image file name obtained in step 1 and image file name of device should be same.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 9: DESCRIPTION : Verify CDL has happened successfully and new image is upgraded.");
	    LOGGER.info("STEP 9: ACTION : Execute SNMP GET command with  oid as 1.3.6.1.2.1.69.1.3.5");
	    LOGGER.info("STEP 9: EXPECTED : SNMP Get should return the image file name of the device.");
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("Waiting for 10 minute to verify upgraded image name after successful download.");
	    tapEnv.waitTill(BroadBandTestConstants.THREE_MINUTE_IN_MILLIS);

	    status = BroadBandCodeDownloadUtils.isImageUpgradedInDevice(tapEnv, device, latestImageNameToUpgrade);

	    if (status) {
		LOGGER.info(
			"STEP 9: ACTUAL : Device build verified to latest build successfully after SNMP HTTP(IPv6) upgrade.");
	    } else {
		LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    /**
	     * Method to set Download Protocol, Server Address Type, Server address, Firmware image for TFTP Ipv4
	     * download
	     */
	    methodToSetParamsForSnmpCodeDownloadTFTPIpv4(tapEnv, device, latestImageNameToUpgrade, testCaseId,
		    BroadBandTestConstants.CONSTANT_10);

	    stepNum = "s14";
	    errorMessage = "snmpset on docsDevSwAdminStatus(1.3.6.1.2.1.69.1.3.3.0) as 1 failed.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 14: DESCRIPTION : Verify SNMP MIB docsDevSwAdminStatus is set for starting the download.");
	    LOGGER.info(
		    "STEP 14: ACTION : Execute SNMP SET command with  oid as 1.3.6.1.2.1.69.1.3.3.0 and value to be set as  1  and SNMP get to verify same set value.");
	    LOGGER.info(
		    "STEP 14: EXPECTED : SNMP MIB  docsDevSwAdminStatus should be set to 1 and SNMP Get output should be same as set value.");
	    LOGGER.info("**********************************************************************************");
	    count = 1;
	    while (count <= 7) {
		snmpSetResponse = BroadBandSnmpUtils.executeSnmpSetWithTableIndexOnRdkDevices(tapEnv, device,
			BroadBandSnmpMib.ECM_DOCS_DEVSW_ADMIN_STATAUS_WITHOUT_INDEX.getOid(), SnmpDataType.INTEGER,
			BroadBandTestConstants.STRING_VALUE_ONE, BroadBandTestConstants.STRING_ZERO);
		status = BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
			BroadBandTestConstants.STRING_VALUE_ONE, snmpSetResponse);
		if (status) {
		    break;
		}
		tapEnv.waitTill(BroadBandTestConstants.THREE_MINUTE_IN_MILLIS);
		count++;
	    }

	    if (status) {
		LOGGER.info("STEP 14: ACTUAL : SNMP MIB firmwareupdate and factory reset is set to 1 successfully.");
	    } else {
		LOGGER.error("STEP 14: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "s15";
	    errorMessage = "SNMP code download started even after giving device build as current build.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 15: DESCRIPTION : Verify SNMP code download has not started.");
	    LOGGER.info("STEP 15: ACTION : Execute SNMP GET command with  oid as 1.3.6.1.2.1.69.1.3.4.0.");
	    LOGGER.info("STEP 15: EXPECTED : SNMP Get should return the Integer value 4 or 5.");
	    LOGGER.info("**********************************************************************************");
	    status = !BroadBandCodeDownloadUtils.isUpgradeStatusInProgressUsingSnmpCommand(tapEnv, device);
	    if (!status) {
		LOGGER.info("Code download started. Will verify for code download failed.");
		status = !BroadBandCodeDownloadUtils.verifySnmpCodeDownlaodCompletionStatus(tapEnv, device);
	    }
	    if (status) {
		LOGGER.info(
			"STEP 15: ACTUAL : SNMP code download via TFTP(Ipv4) server has not started as build to be upgrade is given as device current build.");
	    } else {
		LOGGER.error("STEP 15: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "s16";
	    errorMessage = "Webpa is not up for device after code download.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 16: DESCRIPTION : Verify SNMP MIB FACTORY_RESET_DEVICE is set for Factory Resetting the device.");
	    LOGGER.info(
		    "STEP 16: ACTION : Execute SNMP SET command with  oid as 1.3.6.1.4.1.17270.50.2.1.1.1002.0 and value to be set as 1.");
	    LOGGER.info("STEP 16: EXPECTED : SNMP MIB FACTORY_RESET_DEVICE should be set to 1.");
	    LOGGER.info("**********************************************************************************");
	    if (BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true)) {
		errorMessage = "snmpset on FACTORY_RESET_DEVICE(1.3.6.1.4.1.17270.50.2.1.1.1002.0) as 1 failed.";
		String sysUpTime = tapEnv.executeWebPaCommand(device,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_UPTIME);
		int uptimeInMin = Integer.parseInt(sysUpTime) / 60;
		if (uptimeInMin > 21) {
		    LOGGER.info("Device uptime is more than 20 min.");
		} else {
		    int WaitTime = 21 - uptimeInMin;
		    LOGGER.error("Device uptime is " + uptimeInMin + ". Will wait for " + WaitTime + " min.");
		    tapEnv.waitTill(WaitTime * 60000);
		    LOGGER.info("Wait time is over.");
		}
		snmpSetResponse = BroadBandSnmpUtils.executeSnmpSetWithTableIndexOnRdkDevices(tapEnv, device,
			BroadBandSnmpMib.FACTORY_RESET_DEVICE.getOid(), SnmpDataType.INTEGER,
			BroadBandTestConstants.STRING_VALUE_ONE, BroadBandTestConstants.STRING_ZERO);
		status = BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
			BroadBandTestConstants.STRING_VALUE_ONE, snmpSetResponse);
		if (status) {
		    LOGGER.info(
			    "SNMP MIB FACTORY_RESET_DEVICE is set to 1 successfully.Will wait for device to go for reboot.");
		    errorMessage = "Device didn't go for reboot.";
		    status = CommonMethods.isSTBRebooted(tapEnv, device, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS,
			    BroadBandTestConstants.ESTB_SW_UPDATE_STATUS_LOOP_COUNT);
		    if (status) {
			LOGGER.info("Device rebooted successfully.");
			errorMessage = "Device is not coming up after successful Factory Reset.";
			status = BroadBandCommonUtils.isSTBAccessible(tapEnv, device,
				BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS,
				BroadBandTestConstants.CONFIGURED_TEXTURES_CACHE_LIMIT_IN_MB);
			if (status) {
			    LOGGER.info("Device came up successfully after factory reset.");
			    errorMessage = "Webpa is not up after successful factory reset.";
			    if (BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true)) {
				LOGGER.info("Webpa is up and running after successful factory reset.");
				errorMessage = "Device reboot reason verified successfully.";
				status = FirmwareDownloadUtils.verifyLastRebootReasonViaWebpa(tapEnv, device,
					BroadBandTestConstants.REBOOT_REASON_FACTORY_RESET);
			    }

			}
		    }
		}
	    }
	    if (status) {
		LOGGER.info(
			"STEP 16: ACTUAL : SNMP MIB FACTORY_RESET_DEVICE is set to 1 successfully and Factory Reset happened successfully.");
	    } else {
		LOGGER.error("STEP 16: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	} catch (Exception exception) {
	    errorMessage = errorMessage + exception.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    int postConditionStepNum = 0;

	    status = false;
	    String imageAfterTriggering = "";
	    errorMessage = "Device is not accessible even after waiting for 10 mins.";
	    String successMessage = "";
	    postConditionStepNum++;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("POST-CONDITION " + postConditionStepNum
		    + ": DESCRIPTION : Verify reverting device to original image.");
	    LOGGER.info("POST-CONDITION " + postConditionStepNum
		    + ": ACTION : Flash the original build on the device using HTTP/ TR-181.");
	    LOGGER.info("POST-CONDITION " + postConditionStepNum
		    + ": EXPECTED : Device should be upgraded to original image.");
	    LOGGER.info("**********************************************************************************");
	    try {
		if (BroadBandCommonUtils.isSTBAccessible(tapEnv, device, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
			BroadBandTestConstants.CONSTANT_10)) {
		    errorMessage = "Webap is not up and running.";
		    if (BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true)) {
			errorMessage = "Unable to get current firmware version.";
			imageAfterTriggering = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);
			if (CommonMethods.isNotNull(imageAfterTriggering)
				&& CommonMethods.isNotNull(currentImageName)) {
			    if (!imageAfterTriggering.equals(currentImageName)) {
				status = BroadBandCodeDownloadUtils.upgradeDeviceWithGivenFirmwareVersion(device,
					tapEnv, currentImageName);
				if (!status) {
				    status = BroadBandCodeDownloadUtils.triggerPreviousCodeDownload(device, tapEnv,
					    currentImageName);
				}
			    } else {
				successMessage = "Device Build hasn't changed so need to revert the device image.";
				status = true;
			    }
			}
		    }
		}
	    } catch (Exception e) {
		errorMessage = "Exception occured during reverting the device back to original image." + errorMessage
			+ e.getMessage();
	    }
	    if (status) {
		LOGGER.info("POST-CONDITION " + postConditionStepNum + ": ACTUAL : " + successMessage);
	    } else {
		LOGGER.error("POST-CONDITION " + postConditionStepNum + ": ACTUAL : " + errorMessage);
	    }

	    /**
	     * POST-CONDITION 2 : REACTIVATE DEVICE
	     */

	    BroadBandPostConditionUtils.executePostConditionToReActivateDevice(device, tapEnv, false,
		    BroadBandTestConstants.CONSTANT_2);

	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-WH-SNMP-1006");
    }

    /**
     *
     * Test Case : Improved testing sequences for RDK-B WIFI warehouse
     *
     * <p>
     * STEPS:
     * </p>
     * <ol>
     * <li>Step 1: Perform reboot on the device</li>
     * <li>Step 2: Verify device is up</li>
     * <li>Step 3: Perform factory reset on the device</li>
     * <li>Step 4: Verify device is up</li>
     * <li>Step 5: Validate default Mac Address and password length after factory reset</li>
     * <li>Step 6: Verify all wifi's are on</li>
     * <li>Step 7: Modify the default ssid's to custom ssid's</li>
     * <li>Step 8: Disable RDKB-SSID's</li>
     * <li>Step 9: Perform reboot on the device</li>
     * <li>Step 10: Verify device is up</li>
     * <li>Step 11: Verify all wifi's are off</li>
     * <li>Step 12: Verify 2.4 Ghz radio is enabled</li>
     * <li>Step 13: Verify 2.4 Ghz ssid is enabled</li>
     * <li>Step 14: Verify 2.4 Ghz ssid and password</li>
     * <li>Step 15: Verify 5 Ghz radio is enabled</li>
     * <li>Step 16: Verify 5 Ghz ssid is enabled</li>
     * <li>Step 17: Verify 5 Ghz ssid and password</li>
     * <li>Step 18: Enable RDKB-SSID's</li>
     * <li>Step 19: Perform wifi reset on the device</li>
     * <li>Post Condition:Reativate the device using SNMP</li>
     * </ol>
     * 
     * @param device
     *            Dut instance
     * 
     * @author Muthukumar
     * @refactor Govardhan
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WH-FR-RS-5001")
    public void testToVerifyRdkbWareHouseSequence(Dut device) {
	// Variable declaration starts
	String testCaseId = "TC-RDKB-WH-FR-RS-501";
	String stepNum = "S1";
	String errorMessage = null;
	boolean status = false;
	// Variable declaration ends
	String ssidFor2Ghz = null;
	String ssidFor5Ghz = null;
	String passwordFor2Ghz = null;
	String passwordFor5Ghz = null;
	String response = null;
	boolean isResetDone = false;
	String snmpCommandOutput = null;

	LOGGER.info("##########################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-WH-FR-RS-5001");
	LOGGER.info("TEST DESCRIPTION: Improved testing sequences for RDK-B WIFI warehouse");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("Step 1: Perform reboot on the device");
	LOGGER.info("Step 2: Verify device is up");
	LOGGER.info("Step 3: Perform factory reset on the device");
	LOGGER.info("Step 4: Verify device is up");
	LOGGER.info("Step 5: Validate default Mac Address and password length after factory reset");
	LOGGER.info("Step 6: Verify all wifi's are on");
	LOGGER.info("Step 7: Modify the default ssid's to custom ssid's");
	LOGGER.info("Step 8: Disable RDKB-SSID's");
	LOGGER.info("Step 9: Perform reboot on the device");
	LOGGER.info("Step 10: Verify device is up");
	LOGGER.info("Step 11: Verify all wifi's are off");
	LOGGER.info("Step 12: Verify 2.4 Ghz radio is enabled");
	LOGGER.info("Step 13: Verify 2.4 Ghz ssid is enabled");
	LOGGER.info("Step 14: Verify 2.4 Ghz ssid and password");
	LOGGER.info("Step 15: Verify 5 Ghz radio is enabled");
	LOGGER.info("Step 16: Verify 5 Ghz ssid is enabled");
	LOGGER.info("Step 17: Verify 5 Ghz ssid and password");
	LOGGER.info("Step 18: Enable RDKB-SSID's");
	LOGGER.info("Step 19: Perform wifi reset on the device");
	LOGGER.info("Post Condition:Reativate the device using SNMP");
	LOGGER.info("########################### STARTING SNMP OID : WEBPA PARAMETER ###########################");
	LOGGER.info(BroadBandCommonUtils.concatStringUsingStringBuffer(
		BroadBandSnmpMib.ECM_STATUS_PRIVATE_WIFI_2_4_GHZ.getName(), "=>",
		BroadBandSnmpMib.ECM_STATUS_PRIVATE_WIFI_2_4_GHZ.getOid(), BroadBandTestConstants.DOT_OPERATOR,
		BroadBandSnmpMib.ECM_STATUS_PRIVATE_WIFI_2_4_GHZ.getTableIndex())
		+ "=>"
		+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_ENABLE.replace(
			BroadBandTestConstants.TR181_NODE_REF,
			BroadBandSnmpMib.ECM_STATUS_PRIVATE_WIFI_2_4_GHZ.getTableIndex()));
	LOGGER.info(BroadBandCommonUtils.concatStringUsingStringBuffer(
		BroadBandSnmpMib.ECM_STATUS_PRIVATE_WIFI_5_GHZ.getName(), "=>",
		BroadBandSnmpMib.ECM_STATUS_PRIVATE_WIFI_5_GHZ.getOid(), BroadBandTestConstants.DOT_OPERATOR,
		BroadBandSnmpMib.ECM_STATUS_PRIVATE_WIFI_5_GHZ.getTableIndex())
		+ "=>"
		+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_ENABLE.replace(
			BroadBandTestConstants.TR181_NODE_REF,
			BroadBandSnmpMib.ECM_STATUS_PRIVATE_WIFI_5_GHZ.getTableIndex()));
	LOGGER.info(BroadBandCommonUtils.concatStringUsingStringBuffer(
		BroadBandSnmpMib.HOME_SECURITY_2_4_SSID_STATUS.getName(), "=>",
		BroadBandSnmpMib.HOME_SECURITY_2_4_SSID_STATUS.getOid(), BroadBandTestConstants.DOT_OPERATOR,
		BroadBandSnmpMib.HOME_SECURITY_2_4_SSID_STATUS.getTableIndex())
		+ "=>"
		+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_ENABLE.replace(
			BroadBandTestConstants.TR181_NODE_REF,
			BroadBandSnmpMib.HOME_SECURITY_2_4_SSID_STATUS.getTableIndex()));
	LOGGER.info(BroadBandCommonUtils.concatStringUsingStringBuffer(
		BroadBandSnmpMib.HOME_SECURITY_5_SSID_STATUS.getName(), "=>",
		BroadBandSnmpMib.HOME_SECURITY_5_SSID_STATUS.getOid(), BroadBandTestConstants.DOT_OPERATOR,
		BroadBandSnmpMib.HOME_SECURITY_5_SSID_STATUS.getTableIndex())
		+ "=>"
		+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_ENABLE.replace(
			BroadBandTestConstants.TR181_NODE_REF,
			BroadBandSnmpMib.HOME_SECURITY_5_SSID_STATUS.getTableIndex()));
	LOGGER.info(
		BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandSnmpMib.HOT_SPOT_2_4_SSID_STATUS.getName(),
			"=>", BroadBandSnmpMib.HOT_SPOT_2_4_SSID_STATUS.getOid(), BroadBandTestConstants.DOT_OPERATOR,
			BroadBandSnmpMib.HOT_SPOT_2_4_SSID_STATUS.getTableIndex())
			+ "=>"
			+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_ENABLE.replace(
				BroadBandTestConstants.TR181_NODE_REF,
				BroadBandSnmpMib.HOT_SPOT_2_4_SSID_STATUS.getTableIndex()));
	LOGGER.info(
		BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandSnmpMib.HOT_SPOT_5_SSID_STATUS.getName(),
			"=>", BroadBandSnmpMib.HOT_SPOT_5_SSID_STATUS.getOid(), BroadBandTestConstants.DOT_OPERATOR,
			BroadBandSnmpMib.HOT_SPOT_5_SSID_STATUS.getTableIndex())
			+ "=>"
			+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_ENABLE.replace(
				BroadBandTestConstants.TR181_NODE_REF,
				BroadBandSnmpMib.HOT_SPOT_5_SSID_STATUS.getTableIndex()));
	LOGGER.info(BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandSnmpMib.LNF_2_4_SSID_STATUS.getName(),
		"=>", BroadBandSnmpMib.LNF_2_4_SSID_STATUS.getOid(), BroadBandTestConstants.DOT_OPERATOR,
		BroadBandSnmpMib.LNF_2_4_SSID_STATUS.getTableIndex()) + "=>"
		+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_ENABLE.replace(
			BroadBandTestConstants.TR181_NODE_REF, BroadBandSnmpMib.LNF_2_4_SSID_STATUS.getTableIndex()));
	LOGGER.info(BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandSnmpMib.LNF_5_SSID_STATUS.getName(),
		"=>", BroadBandSnmpMib.LNF_5_SSID_STATUS.getOid(), BroadBandTestConstants.DOT_OPERATOR,
		BroadBandSnmpMib.LNF_5_SSID_STATUS.getTableIndex()) + "=>"
		+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_ENABLE.replace(
			BroadBandTestConstants.TR181_NODE_REF, BroadBandSnmpMib.LNF_5_SSID_STATUS.getTableIndex()));

	LOGGER.info("########################### STARTING SNMP OID : WEBPA PARAMETER ###########################");
	LOGGER.info("##########################################################################");
	try {
	    /**
	     * STEP 1 : PERFORM REBOOT ON THE DEVICE
	     */
	    stepNum = "S1";
	    errorMessage = "Failed to Perform device reboot";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Perform reboot on the device");
	    LOGGER.info("STEP 1: ACTION : Execute command : /sbin/reboot");
	    LOGGER.info("STEP 1: EXPECTED : device should be powered on");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandSnmpUtils.rebootUsingCmdAndWaitForStbAccessibleUsingSnmp(device, tapEnv);
	    if (status) {
		LOGGER.info("STEP 1: ACTUAL :Device rebooted successfully");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    /**
	     * STEP 2 : VERIFY SYSTEM UP TIME
	     */
	    stepNum = "S2";
	    errorMessage = "Failed to get the system up time";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION : Verify system up time");
	    LOGGER.info(
		    "STEP 2: ACTION : Execute snmp get command with uid 1.3.6.1.2.1.1.3.0");
	    LOGGER.info("STEP 2: EXPECTED : Must return the system up time");
	    LOGGER.info("**********************************************************************************");
	    snmpCommandOutput = BroadBandSnmpUtils.executeSnmpWalkOnRdkDevices(tapEnv, device,
		    BroadBandSnmpMib.ESTB_SYS_UP_TIME.getOid());
	    status = BroadBandSnmpUtils.hasNoSNMPErrorOnResponse(tapEnv, device, snmpCommandOutput)
		    && CommonMethods.patternMatcher(snmpCommandOutput, BroadBandTestConstants.SYS_UP_TIME_INSTANCE);
	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : Successfully verified device is Up");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    /**
	     * STEP 3 : PERFORM FACTORY RESET ON THE DEVICE
	     */
	    stepNum = "S3";
	    errorMessage = "Failed to perform factory reset";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION : Perform factory reset on the device");
	    LOGGER.info(
		    "STEP 3: ACTION : Execute snmp set command using uid 1.3.6.1.4.1.17270.50.2.1.1.1002.0 i 1");
	    LOGGER.info("STEP 3: EXPECTED : Device must go for factory reset");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandSnmpUtils.performResetUsingSnmp(tapEnv, device,
		    BroadBandTestConstants.BOOLEAN_VALUE_TRUE);
	    if (status) {
		isResetDone = status;
		LOGGER.info("STEP 3: ACTUAL : Factory reset performed successfully using SNMP");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    /**
	     * STEP 4 : VERIFY SYSTEM UP TIME
	     */
	    stepNum = "S4";
	    errorMessage = "Failed to get the system up time";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 4: DESCRIPTION : Verify system up time");
	    LOGGER.info(
		    "STEP 4: ACTION : Execute snmp get command using uid 1.3.6.1.2.1.1.3.0");
	    LOGGER.info("STEP 4: EXPECTED : Must return the system up time");
	    LOGGER.info("**********************************************************************************");
	    snmpCommandOutput = BroadBandSnmpUtils.executeSnmpWalkOnRdkDevices(tapEnv, device,
		    BroadBandSnmpMib.ESTB_SYS_UP_TIME.getOid());
	    status = BroadBandSnmpUtils.hasNoSNMPErrorOnResponse(tapEnv, device, snmpCommandOutput)
		    && CommonMethods.patternMatcher(snmpCommandOutput, BroadBandTestConstants.SYS_UP_TIME_INSTANCE);
	    if (status) {
		LOGGER.info("STEP 4: ACTUAL : Successfully verified device is Up");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    /**
	     * STEP 5 : VALIDATE DEFAULT MAC ADDRESS AND PASSWORD LENGTH AFTER FACTORY RESET
	     */
	    stepNum = "S5";
	    errorMessage = "Failed to validate default ssid's and password length after factory reset";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 5: DESCRIPTION : Validate default wifi ssid's and password length after factory reset");
	    LOGGER.info("STEP 5: ACTION : Exeucte Snmp Command \n"
		    + "1. Verify the 2.4 SSID snmp get using 1.3.6.1.4.1.17270.50.2.2.2.1.1.3.10001 \n"
		    + "2. Verify the 5 SSID snmp get using 1.3.6.1.4.1.17270.50.2.2.2.1.1.3.10101 \n"
		    + "3. Verify the 2.4 Password snmp get using 1.3.6.1.4.1.17270.50.2.2.3.3.1.3.10001 \n"
		    + "4. Verify the 5 Password snmp get using 1.3.6.1.4.1.17270.50.2.2.3.3.1.3.10101");
	    LOGGER.info("STEP 5: EXPECTED : Must return the default SSID and password length");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandSnmpUtils.verifyDefaultSsidAndPasswordUsingSnmp(tapEnv, device);
	    if (status) {
		LOGGER.info("STEP 5: ACTUAL : Successfully verified the default ssid's and password length");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    /**
	     * STEP 6 : VERIFY ALL WIFI'S ARE ON
	     */
	    stepNum = "S6";
	    errorMessage = "Failed to verify the wifi status";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 6: DESCRIPTION : Verify all wifi's are on");
	    LOGGER.info("STEP 6: ACTION : Exeucte Snmp Command:\n"
		    + "snmp get using using uid .1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10001\n"
		    + "snmp get using uid .1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10002\n"
		    + "snmp get using uid .1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10004\n"
		    + "snmp get using uid .1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10006\n"
		    + "snmp get using uid .1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10101\n"
		    + "snmp get using uid .1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10102\n"
		    + "snmp get using uid .1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10104\n"
		    + "snmp get using uid .1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10106");
	    LOGGER.info("STEP 6: EXPECTED : Must return the value as 1");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandSnmpUtils.setOrVerifyAllSsidStatusUsingSnmp(tapEnv, device,
		    BroadBandTestConstants.STRING_CONSTANT_1, BroadBandTestConstants.BOOLEAN_VALUE_FALSE, true);
	    if (status) {
		LOGGER.info("STEP 6: ACTUAL : Successfully verified the all wifi status its return 1");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    /**
	     * STEP 7 : MODIFY THE DEFAULT SSID'S TO CUSTOM SSID'S
	     */
	    stepNum = "S7";
	    errorMessage = "Failed to modify the SSID's";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 7: DESCRIPTION : Modify the default ssid's to custom ssid's");
	    LOGGER.info("STEP 7: ACTION : Execute Snmp Command to modify default ssid");
	    LOGGER.info("STEP 7: EXPECTED : Must set the custom ssid's");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandSnmpUtils.setAndVerifyTheCustomSsidAndPassword(tapEnv, device);
	    if (status) {
		ssidFor2Ghz = BroadBandSnmpUtils.getSsidUsingSnmp(tapEnv, device, BroadBandTestConstants.BAND_2_4GHZ);
		LOGGER.info("Customized SSID for 2.4 GHz :" + ssidFor2Ghz);
		ssidFor5Ghz = BroadBandSnmpUtils.getSsidUsingSnmp(tapEnv, device, BroadBandTestConstants.BAND_5GHZ);
		LOGGER.info("Customized SSID for 5 GHz :" + ssidFor5Ghz);
		passwordFor2Ghz = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
			BroadBandSnmpMib.ECM_PRIVATE_WIFI_2_4_WIRELESSPASS.getOid(),
			BroadBandSnmpMib.ECM_PRIVATE_WIFI_2_4_WIRELESSPASS.getTableIndex());
		LOGGER.info("Customized Password for 2.4 GHz :" + passwordFor2Ghz);
		passwordFor5Ghz = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
			BroadBandSnmpMib.ECM_PRIVATE_WIFI_5_WIRELESSPASS.getOid(),
			BroadBandSnmpMib.ECM_PRIVATE_WIFI_5_WIRELESSPASS.getTableIndex());
		LOGGER.info("Customized Password for 5 GHz :" + passwordFor5Ghz);
		LOGGER.info("STEP 7: ACTUAL : Successfully modified the deafut ssid's to custom ssid's");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    /**
	     * STEP 8 : DISABLE RDKB-SSID'S
	     */
	    stepNum = "S8";
	    errorMessage = "Failed to disable RDKB-SSID";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 8: DESCRIPTION : Disable RDKB-SSID's");
	    LOGGER.info("STEP 8: ACTION : Execute Snmp Command Disable RDKB-SSID's ");
	    LOGGER.info("STEP 8: EXPECTED : Must disable the RDKB-SSID's");
	    LOGGER.info("**********************************************************************************");
	    if (BroadBandSnmpUtils.setOrVerifyAllSsidStatusUsingSnmp(tapEnv, device,
		    BroadBandTestConstants.STRING_CONSTANT_2, BroadBandTestConstants.BOOLEAN_VALUE_TRUE, false)) {
		errorMessage = "Failed to disable RDKB-SSID via SNMP set is success,But Failed to verify the set value";
		status = BroadBandSnmpUtils.setOrVerifyAllSsidStatusUsingSnmp(tapEnv, device,
			BroadBandTestConstants.STRING_CONSTANT_2, BroadBandTestConstants.BOOLEAN_VALUE_FALSE, false);
	    }
	    if (status) {
		LOGGER.info("STEP 8: ACTUAL : Successfully disabled the RDKB-SSID's");
	    } else {
		LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    /**
	     * STEP 9 : PERFORM REBOOT ON THE DEVICE
	     */
	    stepNum = "S9";
	    errorMessage = "Failed to reboot the device";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 9: DESCRIPTION : Perform reboot on the device");
	    LOGGER.info("STEP 9: ACTION : Execute command : /sbin/reboot");
	    LOGGER.info("STEP 9: EXPECTED : device should be powered on");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandSnmpUtils.rebootUsingCmdAndWaitForStbAccessibleUsingSnmp(device, tapEnv);
	    if (status) {
		LOGGER.info("STEP 9: ACTUAL :Device rebooted successfully");
	    } else {
		LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    /**
	     * STEP 10 : VERIFY SYSTEM UP TIME
	     */
	    stepNum = "S10";
	    errorMessage = "Failed to get the system up time";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 10: DESCRIPTION : Verify system up time");
	    LOGGER.info(
		    "STEP 10: ACTION : Execute snmp command snmpget using OID 1.3.6.1.2.1.1");
	    LOGGER.info("STEP 10: EXPECTED : Must return the system up time");
	    LOGGER.info("**********************************************************************************");
	    snmpCommandOutput = BroadBandSnmpUtils.executeSnmpWalkOnRdkDevices(tapEnv, device,
		    BroadBandSnmpMib.ESTB_SYS_UP_TIME.getOid());
	    status = BroadBandSnmpUtils.hasNoSNMPErrorOnResponse(tapEnv, device, snmpCommandOutput)
		    && CommonMethods.patternMatcher(snmpCommandOutput, BroadBandTestConstants.SYS_UP_TIME_INSTANCE);
	    if (status) {
		LOGGER.info("STEP 10: ACTUAL : Successfully verified the system uptime");
	    } else {
		LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    /**
	     * STEP 11 : VERIFY ALL WIFI'S ARE OFF
	     */
	    stepNum = "S11";
	    errorMessage = "Failed to verify the wifi status";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 11: DESCRIPTION : Verify all wifi's are off");
	    LOGGER.info("STEP 11: ACTION : Exeucte Snmp Commands to verify all wifi's are off");
	    LOGGER.info("STEP 11: EXPECTED : Must return the value as 2");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandSnmpUtils.setOrVerifyAllSsidStatusUsingSnmp(tapEnv, device,
		    BroadBandTestConstants.STRING_CONSTANT_2, BroadBandTestConstants.BOOLEAN_VALUE_FALSE, false);
	    if (status) {
		LOGGER.info("STEP 11: ACTUAL : Successfully verified the all wifi status and its return 2");
	    } else {
		LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    /**
	     * STEP 12 : VERIFY 2.4 GHZ RADIO IS ENABLED
	     */
	    stepNum = "S12";
	    errorMessage = "Failed to verify 2.4 Ghz radio is enabled";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 12: DESCRIPTION : Verify 2.4 Ghz radio is enabled");
	    LOGGER.info(
		    "STEP 12: ACTION : Execute Snmp Command snmpget using OID 1.3.6.1.4.1.17270.50.2.2.6.1.1.1.10000");
	    LOGGER.info("STEP 12: EXPECTED : Must return the output as 3");
	    LOGGER.info("**********************************************************************************");
	    String mibWithOutIndex = BroadBandSnmpMib.ECM_WIFI_2_4_SSID_RADIO_STATUS_ENABLE.getOid()
		    .substring(BroadBandTestConstants.CONSTANT_1, BroadBandSnmpMib.ECM_WIFI_2_4_SSID_RADIO_STATUS_ENABLE
			    .getOid().lastIndexOf(BroadBandTestConstants.DOT_OPERATOR));
	    String tableIndex = BroadBandSnmpMib.ECM_WIFI_2_4_SSID_RADIO_STATUS_ENABLE.getOid()
		    .substring(BroadBandSnmpMib.ECM_WIFI_2_4_SSID_RADIO_STATUS_ENABLE.getOid()
			    .lastIndexOf(BroadBandTestConstants.DOT_OPERATOR) + BroadBandTestConstants.CONSTANT_1);
	    long startTime = System.currentTimeMillis();
	    do {
		status = BroadBandSnmpUtils.performSnmpGetOnRdkDevicesAndVerify(tapEnv, device, mibWithOutIndex,
			tableIndex, BroadBandTestConstants.STRING_VALUE_THREE);
	    } while (!status && (System.currentTimeMillis() - startTime) < BroadBandTestConstants.EIGHT_MINUTE_IN_MILLIS
		    && BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    if (status) {
		LOGGER.info("STEP 12: ACTUAL : Successfully verified the 2.4 Ghz radio status");
	    } else {
		LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    /**
	     * STEP 13 : VERIFY 2.4 GHZ SSID IS ENABLED
	     */
	    stepNum = "S13";
	    errorMessage = "Failed to verify 2.4 Ghz ssid enabled status";

	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 13: DESCRIPTION : Verify 2.4 Ghz ssid  is enabled");
	    LOGGER.info("STEP 13: ACTION : Execute Snmp Command \n"
		    + "snmp get using oid .1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10001 \n"
		    + "	snmp set using oid.1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10001 i 1");
	    LOGGER.info("STEP 13: EXPECTED : 2.4 GHz ssid must be enabled");
	    LOGGER.info("**********************************************************************************");
	    response = BroadBandSnmpUtils.executeSnmpSetWithTableIndexOnRdkDevices(tapEnv, device,
		    BroadBandSnmpMib.ECM_STATUS_PRIVATE_WIFI_2_4_GHZ.getOid(), SnmpDataType.INTEGER,
		    BroadBandTestConstants.STRING_CONSTANT_1,
		    BroadBandSnmpMib.ECM_STATUS_PRIVATE_WIFI_2_4_GHZ.getTableIndex());
	    status = CommonMethods.isNotNull(response)
		    && response.equalsIgnoreCase(BroadBandTestConstants.STRING_CONSTANT_1);
	    if (status) {
		LOGGER.info("STEP 13: ACTUAL : Successfully verified 2.4 Ghz ssid status");
	    } else {
		LOGGER.error("STEP 13: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    /**
	     * STEP 14 : VERIFY 2.4 GHZ SSID AND PASSWORD
	     */
	    stepNum = "S14";
	    errorMessage = "Failed to verify 2.4 Ghz ssid and password";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 14: DESCRIPTION : Verify 2.4 Ghz ssid and password");
	    LOGGER.info("STEP 14: ACTION : Execute Snmp Command to verify 2.4 Ghz ssid and password");
	    LOGGER.info("STEP 14: EXPECTED : Must return the 2.4 GHz ssid and password");
	    LOGGER.info("**********************************************************************************");
	    mibWithOutIndex = BroadBandSnmpMib.ECM_WIFI_SSID_2_4.getOid().substring(BroadBandTestConstants.CONSTANT_1,
		    BroadBandSnmpMib.ECM_WIFI_SSID_2_4.getOid().lastIndexOf(BroadBandTestConstants.DOT_OPERATOR));
	    tableIndex = BroadBandSnmpMib.ECM_WIFI_SSID_2_4.getOid().substring(
		    BroadBandSnmpMib.ECM_WIFI_SSID_2_4.getOid().lastIndexOf(BroadBandTestConstants.DOT_OPERATOR)
			    + BroadBandTestConstants.CONSTANT_1);
	    LOGGER.info("Customized SSID for 2.4 GHz :" + ssidFor2Ghz);
	    LOGGER.info("Customized Password for 2.4 GHz :" + passwordFor2Ghz);
	    status = BroadBandSnmpUtils.performSnmpGetOnRdkDevicesAndVerify(tapEnv, device, mibWithOutIndex, tableIndex,
		    ssidFor2Ghz)
		    && BroadBandSnmpUtils.performSnmpGetOnRdkDevicesAndVerify(tapEnv, device,
			    BroadBandSnmpMib.ECM_PRIVATE_WIFI_2_4_WIRELESSPASS.getOid(),
			    BroadBandSnmpMib.ECM_PRIVATE_WIFI_2_4_WIRELESSPASS.getTableIndex(), passwordFor2Ghz);
	    if (status) {
		LOGGER.info("STEP 14: ACTUAL : Successfully verified the 2.4 Ghz ssid and password");
	    } else {
		LOGGER.error("STEP 14: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    /**
	     * STEP 15 : VERIFY 5 GHZ RADIO IS ENABLED
	     */
	    stepNum = "S15";
	    errorMessage = "Failed to verify 5 Ghz radio is enabled";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 15: DESCRIPTION : Verify 5 Ghz radio is enabled");
	    LOGGER.info(
		    "STEP 15: ACTION : Execute Snmp Command snmp get using OID 1.3.6.1.4.1.17270.50.2.2.6.1.1.1.10100");
	    LOGGER.info("STEP 15: EXPECTED : Must return the output as 3");
	    LOGGER.info("**********************************************************************************");
	    mibWithOutIndex = BroadBandSnmpMib.ECM_WIFI_5_SSID_RADIO_STATUS_ENABLE.getOid()
		    .substring(BroadBandTestConstants.CONSTANT_1, BroadBandSnmpMib.ECM_WIFI_5_SSID_RADIO_STATUS_ENABLE
			    .getOid().lastIndexOf(BroadBandTestConstants.DOT_OPERATOR));
	    tableIndex = BroadBandSnmpMib.ECM_WIFI_5_SSID_RADIO_STATUS_ENABLE.getOid()
		    .substring(BroadBandSnmpMib.ECM_WIFI_5_SSID_RADIO_STATUS_ENABLE.getOid()
			    .lastIndexOf(BroadBandTestConstants.DOT_OPERATOR) + BroadBandTestConstants.CONSTANT_1);
	    status = BroadBandSnmpUtils.performSnmpGetOnRdkDevicesAndVerify(tapEnv, device, mibWithOutIndex, tableIndex,
		    BroadBandTestConstants.STRING_VALUE_THREE);
	    if (status) {
		LOGGER.info("STEP 15: ACTUAL : Successfully verified the 5 Ghz radio status");
	    } else {
		LOGGER.error("STEP 15: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    /**
	     * STEP 16 : VERIFY 5 GHZ SSID IS ENABLED
	     */
	    stepNum = "S16";
	    errorMessage = "Failed to verify 5 Ghz ssid enabled status";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 16: DESCRIPTION : Verify 5 Ghz ssid  is enabled");
	    LOGGER.info("STEP 16: ACTION : Execute Snmp Command to check 5 Ghz ssid  is enabled");
	    LOGGER.info("STEP 16: EXPECTED : 2.4 GHz ssid must be enabled");
	    LOGGER.info("**********************************************************************************");
	    response = BroadBandSnmpUtils.executeSnmpSetWithTableIndexOnRdkDevices(tapEnv, device,
		    BroadBandSnmpMib.ECM_STATUS_PRIVATE_WIFI_5_GHZ.getOid(), SnmpDataType.INTEGER,
		    BroadBandTestConstants.STRING_CONSTANT_1,
		    BroadBandSnmpMib.ECM_STATUS_PRIVATE_WIFI_5_GHZ.getTableIndex());
	    status = CommonMethods.isNotNull(response)
		    && response.equalsIgnoreCase(BroadBandTestConstants.STRING_CONSTANT_1);
	    if (status) {
		LOGGER.info("STEP 16: ACTUAL : Successfully verified 5 Ghz ssid status");
	    } else {
		LOGGER.error("STEP 16: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    /**
	     * STEP 17 : VERIFY 5 GHZ SSID AND PASSWORD
	     */
	    stepNum = "S17";
	    errorMessage = "Failed to verify 5 Ghz ssid  and password";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 17: DESCRIPTION : Verify 5 Ghz ssid and password");
	    LOGGER.info("STEP 17: ACTION : Execute Snmp Command: to verify 5GHz SSID and password");
	    LOGGER.info("**********************************************************************************");
	    mibWithOutIndex = BroadBandSnmpMib.ECM_WIFI_SSID_5.getOid().substring(BroadBandTestConstants.CONSTANT_1,
		    BroadBandSnmpMib.ECM_WIFI_SSID_5.getOid().lastIndexOf(BroadBandTestConstants.DOT_OPERATOR));
	    tableIndex = BroadBandSnmpMib.ECM_WIFI_SSID_5.getOid().substring(
		    BroadBandSnmpMib.ECM_WIFI_SSID_5.getOid().lastIndexOf(BroadBandTestConstants.DOT_OPERATOR)
			    + BroadBandTestConstants.CONSTANT_1);
	    LOGGER.info("Customized SSID for 5 GHz :" + ssidFor5Ghz);
	    LOGGER.info("Customized Password for 5 GHz :" + passwordFor5Ghz);
	    status = BroadBandSnmpUtils.performSnmpGetOnRdkDevicesAndVerify(tapEnv, device, mibWithOutIndex, tableIndex,
		    ssidFor5Ghz)
		    && BroadBandSnmpUtils.performSnmpGetOnRdkDevicesAndVerify(tapEnv, device,
			    BroadBandSnmpMib.ECM_PRIVATE_WIFI_5_WIRELESSPASS.getOid(),
			    BroadBandSnmpMib.ECM_PRIVATE_WIFI_5_WIRELESSPASS.getTableIndex(), passwordFor5Ghz);
	    if (status) {
		LOGGER.info("STEP 17: ACTUAL : Successfully verified the 5 Ghz ssid and password");
	    } else {
		LOGGER.error("STEP 17: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    /**
	     * STEP 18 : Enable RDKB-SSID's
	     */
	    stepNum = "S18";
	    errorMessage = "Failed to Enable RDKB-SSID";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 18: DESCRIPTION : Enable RDKB-SSID's");
	    LOGGER.info("STEP 18: ACTION : Execute Snmp Command to enable RDKB-SSID's ");
	    LOGGER.info("STEP 18: EXPECTED : Must enable the RDKB-SSID\"s");
	    LOGGER.info("**********************************************************************************");
	    if (BroadBandSnmpUtils.setOrVerifyAllSsidStatusUsingSnmp(tapEnv, device,
		    BroadBandTestConstants.STRING_CONSTANT_1, BroadBandTestConstants.BOOLEAN_VALUE_TRUE, false)) {
		errorMessage = "Failed to enable RDKB-SSID via SNMP set is success,But Failed to verify the set value";
		status = BroadBandSnmpUtils.setOrVerifyAllSsidStatusUsingSnmp(tapEnv, device,
			BroadBandTestConstants.STRING_CONSTANT_1, BroadBandTestConstants.BOOLEAN_VALUE_FALSE, false);
	    }
	    if (status) {
		LOGGER.info("STEP 18: ACTUAL : Successfully enabled the RDKB-SSID's");
	    } else {
		LOGGER.error("STEP 18: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    /**
	     * STEP 19 : PERFORM WIFI RESET ON THE DEVICE
	     */
	    stepNum = "S19";
	    errorMessage = "Failed to perform wifi reset";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 19: DESCRIPTION : Perform wifi reset on the device");
	    LOGGER.info(
		    "STEP 19: ACTION : Execute snmp command snmp set using OID 1.3.6.1.4.1.17270.50.2.1.1.1002.");
	    LOGGER.info("STEP 19: EXPECTED : Device must go for wif reset");
	    LOGGER.info("**********************************************************************************");
	    if (BroadBandSnmpUtils.performResetUsingSnmp(tapEnv, device, BroadBandTestConstants.BOOLEAN_VALUE_FALSE)) {
		errorMessage = "Failed to verify default SSID's and Password after wifi reset";
		startTime = System.currentTimeMillis();
		do {
		    status = BroadBandSnmpUtils.verifyDefaultSsidAndPasswordUsingSnmp(tapEnv, device);
		} while (!status
			&& (System.currentTimeMillis() - startTime) < BroadBandTestConstants.EIGHT_MINUTE_IN_MILLIS
			&& BroadBandCommonUtils.hasWaitForDuration(tapEnv,
				BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    }
	    if (status) {
		LOGGER.info("STEP 19: ACTUAL : Wifi reset preformed successfully");
	    } else {
		LOGGER.error("STEP 19: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	} finally {
	    if (isResetDone) {
		status = false;
		LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
		LOGGER.info("#######################################################################################");
		LOGGER.info("POST-CONDITION : DESCRIPTION : Reativate the device using SNMP");
		LOGGER.info("POST-CONDITION : ACTION : Execute Snmp Command reativate the device using SNMP");
		LOGGER.info("POST-CONDITION : EXPECTED : Reactivation should be successful ");
		LOGGER.info("#######################################################################################");
		try {
		    BroadBandWiFiUtils.reactivateDeviceUsingSnmp(tapEnv, device);
		    BroadBandSnmpUtils.executeSnmpSetWithTableIndexOnRdkDevices(tapEnv, device,
			    BroadBandSnmpMib.WIFI_APPLY_SETTINGS.getOid(), SnmpDataType.INTEGER,
			    BroadBandTestConstants.STRING_VALUE_ONE, BroadBandTestConstants.STRING_ZERO);
		    LOGGER.info("Waiting for 90 seconds after applying all WiFi settings.");
		    tapEnv.waitTill(BroadBandTestConstants.NINETY_SECOND_IN_MILLIS);
		    status = true;
		} catch (Exception e) {
		    errorMessage = e.getMessage();
		}
		if (status) {
		    LOGGER.info("POST-CONDITION : ACTUAL : Device reactivated successfully using SNMP");
		} else {
		    LOGGER.error("POST-CONDITION : ACTUAL : Failed to reactivate the device using SNMP");
		}
		LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
		LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	    }
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-WH-FR-RS-5001");
    }

}