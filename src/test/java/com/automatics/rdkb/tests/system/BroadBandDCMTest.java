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
package com.automatics.rdkb.tests.system;

import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.codehaus.jackson.map.ObjectMapper;
import org.codehaus.jackson.type.TypeReference;
import org.codehaus.jettison.json.JSONArray;
import org.codehaus.jettison.json.JSONObject;
import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.WebPaParamConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandRfcFeatureControlUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.tr69.BroadBandTr69Utils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.AutomaticsPropertyUtility;
import com.automatics.utils.CommonMethods;

/**
 * Class for Enable Disable Feature Via DCM
 */
public class BroadBandDCMTest extends AutomaticsTestBase {
    
    /** Constant holds the maintenance window mapping **/
    private static Map<String, String> maintenanceWindow = null;
    
    /**
     * Verify configurable RFC check-in trigger using tr181 parameter
     * <ol>
     * <li>Clear dcmrfc.log and trigger Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Control.RetrieveNow</li>
     * <li>Verify tr181 parameter has been enabled by RFC script executed after trigger</li>
     * </ol>
     * 
     * @param device
     *            {@link Dut}
     * 
     * @author Ashwin sankara
     * @refactor Govardhan
     * 
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-RFC-1001")
    public void testVerifyConfigurableRFC(Dut device) {

	// Variable Declaration begins
	String testCaseId = "";
	String stepNum = "";
	String errorMessage = "";
	boolean status = false;
	BroadBandResultObject result = new BroadBandResultObject();
	// Variable Declation Ends

	testCaseId = "TC-RDKB-RFC-101";

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-RFC-1001");
	LOGGER.info("TEST DESCRIPTION: Verify configurable RFC check-in trigger using tr181 parameter");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Clear dcmrfc.log and trigger Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Control.RetrieveNow");
	LOGGER.info("2. Verify tr181 parameter has been enabled by RFC script executed after trigger");

	LOGGER.info("#######################################################################################");

	try {

	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    LOGGER.info(
		    "PRE-CONDITION : DESCRIPTION : Configure RFC payload data to enable tr181 parameter when RFC retrieve now is triggered.");
	    LOGGER.info(
		    "PRE-CONDITION : ACTION : Copy and update RFC_CONFIG_SERVER_URL in /nvram/rfc.properties and post the payload");
	    LOGGER.info("PRE-CONDITION : EXPECTED : Successfully copied file and posted payload data");

	    if (BroadBandRfcFeatureControlUtils.executePreconditionForRfcTests(device, tapEnv,
		    AutomaticsPropertyUtility
			    .getProperty(BroadBandTestConstants.PROP_KEY_TO_ENABLE_DISABLE_CLOUD_UPLOAD_ENCRYPTION)
			    .replace(BroadBandTestConstants.STRING_TO_CHANGE_ENCRYPT_CLOUD_UPLOAD_STATUS,
				    BroadBandTestConstants.TRUE))) {
		LOGGER.info("PRE-CONDITION : ACTUAL : Pre condition executed successfully");
	    }

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s1";
	    errorMessage = "Unable to set value of Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Control.RetrieveNow to unsigned integer 1";
	    status = false;

	    LOGGER.info(
		    "STEP 1: DESCRIPTION : Clear dcmrfc.log and trigger Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Control.RetrieveNow");
	    LOGGER.info(
		    "STEP 1: ACTION : Clear dcmrfc.log and execute webpa command to set Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Control.RetrieveNow");
	    LOGGER.info("STEP 1: EXPECTED : Successfully set RFC.RetrieveNow parameter with unsigned int 1");

	    status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    WebPaParamConstants.WEBPA_PARAM_IMMEDIATE_RFC_CHECK_IN, BroadBandTestConstants.CONSTANT_2,
		    BroadBandTestConstants.STRING_VALUE_ONE);

	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : Successfully set RFC.RetrieveNow parameter with unsigned int 1");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s2";
	    errorMessage = "Failed to set parameter value using TR181 trigger of RFC";
	    status = false;

	    LOGGER.info(
		    "STEP 2: DESCRIPTION : Verify tr181 parameter has been enabled by RFC script executed after trigger");
	    LOGGER.info(
		    "STEP 2: ACTION : Verify parameter value in rfc-parsed.txt, rfc_configdata.txt, dcmrfc.log and tr181 get.");
	    LOGGER.info("STEP 2: EXPECTED : Value of  EncryptUpload Enable parameter value is true");

	    result = BroadBandRfcFeatureControlUtils.verifyParameterUpdatedByRfc(device, tapEnv,
		    WebPaParamConstants.WEBPA_PARAM_TO_ENABLE_DISABLE_CLOUD_UPLOAD_ENCRYPTION,
		    BroadBandTestConstants.TRUE);
	    status = result.isStatus();
	    errorMessage = result.getErrorMessage();

	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : Value of  EncryptUpload Enable parameter value is true");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    true);
	} finally {

	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");
	    LOGGER.info(
		    "POST-CONDITION : DESCRIPTION : Remove nvram override of rfc.properties and disable tr181 parameter.");
	    LOGGER.info("POST-CONDITION : ACTION : Remove /nvram/rfc.properties and disable tr181 parameter.");
	    LOGGER.info("POST-CONDITION : EXPECTED : WebPA set is successful and parmeter value is false");

	    status = CommonUtils.removeFileandVerifyFileRemoval(tapEnv, device,
		    BroadBandRfcFeatureControlUtils.NVRAM_RFC_PROPERTIES)
		    && BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			    WebPaParamConstants.WEBPA_PARAM_TO_ENABLE_DISABLE_CLOUD_UPLOAD_ENCRYPTION,
			    BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.FALSE);

	    if (status) {
		LOGGER.info("POST-CONDITION : ACTUAL : Post condition executed successfully");
	    } else {
		LOGGER.error("POST-CONDITION : ACTUAL : Post condition failed");
	    }
	    LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-RFC-1001");
	LOGGER.info("#######################################################################################");
    }

    /**
     * Test case is created as to Enable Disable Feature Via DCM
     *
     * Test Case # 1: Verify Enable Disable Feature Via DCM after Reboot
     *
     * <p>
     * STEPS:
     * </p>
     * <ol>
     * Precondition: Reboot the device
     * <li>STEP 1: Verify DCM RFC Service started successfully in ArmConsoleLog.txt.0 file</li>
     * <li>STEP 2: Verify Xconf server url in dcmrfc.log and dcm.properties</li>
     * <li>STEP 3: Verify device is querying DCM at startup</li>
     * <li>STEP 4: Verify json message is parsed successfully to file rfcresponse.json</li>
     * <li>STEP 5: Verify list of enabled feature from rfc_configdata.txt</li>
     * <li>STEP 6: Verify GET for TR-181 Parameters through WebPA command</li>
     * <li>STEP 7: Verify DCM response from log file dcmrfc.log</li>
     * </ol>
     *
     * @author Sumathi Gunasekaran
     * @refactor yamini.s
     * 
     * @param device
     *            {@link Dut}
     */

    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-SYSTEM-1102")
    public void testEnableDisableFeatureViaDCMAfterReboot(Dut device) {
	// variable to store errorMessage
	String errorMessage = null;
	// variable to store testcaseID
	String testCaseId = "TC-RDKB-SYSTEM-102";
	// variable to store teststepNumber
	String testStepNumber = "s1";
	// variable to store status
	boolean status = false;
	// variable to store search command
	String searchCommand = null;
	// variable to store command output
	String commandOutput = null;
	// variable to store dcmServerUrl
	String dcmServerUrl = null;
	// Variable to store the response
	String response = null;
	// Variable to store log file name
	String logFileName = null;
	// Map to store Tr181 parameters and its value
	Map<String, Object> tr181ParametersMap = new HashMap<String, Object>();
	// boolean to notify whether rfc props or dcm props is to be used
	boolean hasRfcProps = false;

	try {
	    // Precondition
	    LOGGER.info("### Pre-Condition ### Going to reboot the device.");
	    BroadBandCommonUtils.rebootDeviceAsPreCondition(tapEnv, device);
	    LOGGER.info("Waiting till device uptime is 10 minutes.This is for all the logs to get updated ");
	    BroadBandCommonUtils.waitTillGivenBootupTime(device, tapEnv, BroadBandTestConstants.TEN_MINUTE_IN_MILLIS);
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STARTING TEST CASE: TC-RDKB-SYSTEM-1102");
	    /**
	     * Step 1 : Verify DCM RFC Service started successfully in ArmConsoleLog.txt.0/Console.txt.0 file
	     *
	     */
	    LOGGER.info("***********************************************************");
	    LOGGER.info("STEP 1: Verify DCM RFC Service started successfully in ArmConsoleLog.txt.0 file");
	    LOGGER.info("Expected: Log message should be present in ArmConsolelog.txt.0 file.");
	    LOGGER.info("***********************************************************");
	    logFileName = (CommonMethods.isAtomSyncAvailable(device, tapEnv))
		    ? BroadBandCommandConstants.FILE_ARMCONSOLELOG
		    : BroadBandCommandConstants.FILE_CONSOLELOG;
	    response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTraceConstants.LOG_MESSAGE_DCM_RFC_STARTED, logFileName);
	    LOGGER.info("Response is: " + response);
	    status = CommonUtils.isNotEmptyOrNull(response);
	    errorMessage = "Unable to verify DCM RFC Service in log message.";
	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : DCM RFC Service Started successfully..");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 2 : Verify Xconf server url in dcmrfc.log and dcm.properties
	     *
	     */
	    testStepNumber = "s2";
	    status = false;
	    LOGGER.info("***********************************************************");
	    LOGGER.info("STEP 2: Verify Xconf server url in dcmrfc.log and dcm.properties");
	    LOGGER.info("Expected: The value should be the same in dcm.properties and dcmrfc.log");
	    LOGGER.info("***********************************************************");
	    hasRfcProps = CommonUtils.isFileExists(device, tapEnv, BroadBandRfcFeatureControlUtils.ETC_RFC_PROPERTIES);
	    searchCommand = (hasRfcProps
		    ? BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.CAT_COMMAND,
			    BroadBandRfcFeatureControlUtils.ETC_RFC_PROPERTIES)
		    : BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.GREP_COMMAND,
			    BroadBandTestConstants.DCM_RFC_SERVER_URL_PROPERTY_NAME,
			    BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
			    BroadBandCommandConstants.FILE_DCM_PROPERTIES));
	    LOGGER.info("Search Command is: " + searchCommand);
	    commandOutput = tapEnv.executeCommandUsingSsh(device, searchCommand);
	    if (CommonMethods.isNotNull(commandOutput)) {
		dcmServerUrl = (hasRfcProps
			? CommonMethods.patternFinder(commandOutput,
				BroadBandRfcFeatureControlUtils.PATTERN_FOR_RFC_CONFIG_SERVER_URL)
			: CommonMethods.patternFinder(commandOutput,
				BroadBandTestConstants.REG_EXPRESSION_DCM_SERVER_URL));
		errorMessage = "Unable to verify Xconf server url in log message.";
		if (CommonMethods.isNotNull(dcmServerUrl)) {
		    searchCommand = BroadBandCommonUtils.concatStringUsingStringBuffer(
			    BroadBandTestConstants.GREP_COMMAND, dcmServerUrl,
			    BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandTestConstants.DCMRFC_LOG_FILE);
		    LOGGER.info("Search Command is :" + searchCommand);
		    status = CommonUtils.searchLogFiles(tapEnv, device, searchCommand);
		    errorMessage = "Xconf server url is different in dcm.properties file and in /rdklogs/logs/dcmRFC.log.0.";
		}
	    }

	    if (!status) {
		status = BroadBandRfcFeatureControlUtils.validateRFCConfigURL(device, tapEnv);
	    }
	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : Xconf server url is verified successfully.");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 3 : Verify device is querying DCM at startup
	     *
	     */
	    testStepNumber = "s3";
	    status = false;
	    LOGGER.info("***********************************************************");
	    LOGGER.info("STEP 3: Verify device is querying DCM at startup");
	    LOGGER.info("Expected: Log message should be present in dcmrfc.log");
	    LOGGER.info("***********************************************************");
	    searchCommand = BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.GREP_COMMAND,
		    BroadBandTraceConstants.LOG_MESSAGE_CURL_COMMAND, BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
		    BroadBandCommandConstants.FILE_DCMRFC_LOG);
	    LOGGER.info("Search Command is: " + searchCommand);
	    commandOutput = tapEnv.executeCommandUsingSsh(device, searchCommand);
	    if (CommonMethods.isNotNull(commandOutput)) {
		status = CommonMethods.patternMatcher(commandOutput,
			dcmServerUrl + BroadBandTestConstants.REG_EXPRESSION_DOT_STAR);
		LOGGER.info("Verifying pattern matcher for DCM Server Url: " + status);
	    }
	    errorMessage = "Unable to verify DCM query at startup.";
	    if (status) {
		LOGGER.info("STEP 3: ACTUAL : DCM Query is verified successfully.");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 4 : Verify json message is parsed successfully to file rfcresponse.json
	     *
	     */
	    testStepNumber = "s4";
	    status = false;
	    LOGGER.info("***********************************************************");
	    LOGGER.info("STEP 4: Verify json message is parsed successfully to file rfcresponse.json");
	    LOGGER.info("Expected: The file should not be empty and should display with  json message.");
	    LOGGER.info("***********************************************************");
	    long startTime = System.currentTimeMillis();
	    do {
		tapEnv.waitTill(BroadBandTestConstants.THREE_MINUTE_IN_MILLIS);
		if ((hasRfcProps
			? CommonUtils.isFileExists(device, tapEnv, BroadBandCommandConstants.FILE_RFC_PARSED_TEXT_PATH)
			: CommonUtils.isFileExists(device, tapEnv, BroadBandCommandConstants.FILE_RFC_RESPONSE_LOG))) {
		    searchCommand = (hasRfcProps
			    ? BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.CAT_COMMAND,
				    BroadBandCommandConstants.FILE_RFC_PARSED_TEXT_PATH)
			    : BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.CAT_COMMAND,
				    BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
				    BroadBandCommandConstants.FILE_RFC_RESPONSE_LOG));
		    LOGGER.info("Search Command is: " + searchCommand);
		    commandOutput = tapEnv.executeCommandUsingSsh(device, searchCommand);
		    status = CommonMethods.isNotNull(commandOutput);
		    errorMessage = "Unable to verify json message file in location /tmp/rfcresponse.json.";
		    if (status) {
			tr181ParametersMap = extractWebPaParameterAndValueFromJsonFile(tapEnv, device, commandOutput);
			if (tr181ParametersMap.containsKey(BroadBandTestConstants.RABID_FRAMEWORK_ENABLE)) {
			    tr181ParametersMap.remove(BroadBandTestConstants.RABID_FRAMEWORK_ENABLE);
			}
			status = !tr181ParametersMap.isEmpty();
		    }
		}
	    } while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS
		    && !status);
	    errorMessage = "Unable to verify json message file in location /tmp/rfcresponse.json.";
	    if (status) {
		LOGGER.info("STEP 4: ACTUAL : Verified presence of rfcresponse.json file in /tmp/ directory.");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

	    /**
	     * Step 5 : Verify list of enabled feature from rfc_configdata.txt
	     *
	     */
	    testStepNumber = "s5";
	    status = false;
	    LOGGER.info("***********************************************************");
	    LOGGER.info("STEP 5: Verify list of enabled feature from rfc_configdata.txt");
	    LOGGER.info("Expected:The output should display TR-181 parameters");
	    LOGGER.info("***********************************************************");
	    status = CommonUtils.isFileExists(device, tapEnv, BroadBandCommandConstants.FILE_RFC_CONFIGDATA_LOG);
	    errorMessage = "Unable to verify rfc_configdata.txt file and its parameters.";
	    if (status) {
		status = verifywebPaParameterAndValueInLogFile(tapEnv, device, tr181ParametersMap,
			BroadBandCommandConstants.FILE_RFC_CONFIGDATA_LOG);
		errorMessage = "Failed to verify webpa parameter and its value from rfc_configdata.txt";
	    }
	    if (status) {
		LOGGER.info("STEP 5: ACTUAL : Verified all Enabled features from rfc_configdata.");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

	    /**
	     * Step 6 : Verify GET for TR-181 Parameters through WebPA command
	     *
	     */
	    testStepNumber = "s6";
	    status = false;
	    LOGGER.info("***********************************************************");
	    LOGGER.info("STEP 6: Verify GET for TR-181 Parameters through WebPA command");
	    LOGGER.info("Expected: WebPA should return success message with value for all parameter.");
	    LOGGER.info("***********************************************************");
	    status = verifywebPaParameterAndValueThroughWebPA(tapEnv, device, tr181ParametersMap);
	    errorMessage = "Json ConfigData values are not matching with WebPA Response";
	    if (status) {
		LOGGER.info("STEP 6: ACTUAL : Verified all webPA parameters through webpa command.");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

	    /**
	     * Step 7 : Verify DCM response from log file dcmrfc.log
	     *
	     */
	    testStepNumber = "s7";
	    status = false;
	    LOGGER.info("***********************************************************");
	    LOGGER.info("STEP 7: Verify DCM response from log file dcmrfc.log");
	    LOGGER.info("Expected: Log message should be present in dcmrfc.log");
	    LOGGER.info("***********************************************************");
	    status = verifywebPaParameterAndValueInDcmRfcLog(tapEnv, device, tr181ParametersMap);
	    errorMessage = "Verification of tr181 parameters aganist dcmrfc.log file failed";
	    if (status) {
		LOGGER.info("STEP 7: ACTUAL : Verified DCM response in dcmrfclog file.");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);
	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error(
		    "Exception Occurred while verifying Enable Disable Features Via DCM after reboot:" + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, testStepNumber, status,
		    errorMessage, true);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-SYSTEM-1102");
	LOGGER.info("#######################################################################################");
    }

    /**
     * Method to extract webpa parameter and value from Json file
     * 
     * @param tapEnv
     *            {@link AutomaticsTapApi} Reference
     * @param device
     *            device
     * @param fileName
     *            fileName used in find command
     *
     * @author Sumathi Gunasekaran
     * @refactor yamini.s
     * 
     */

    @SuppressWarnings("unchecked")
    public static Map<String, Object> extractWebPaParameterAndValueFromJsonFile(AutomaticsTapApi tapEnv, Dut device,
	    String jsoncommandOutput) {
	LOGGER.debug("Starting Method:extractWebPaParameterAndValueFromJsonFile ");
	int counter = 0;
	ObjectMapper mapper = new ObjectMapper();
	Map<String, Object> tr181ParametersMap = new HashMap<String, Object>();
	try {
	    JSONObject jsonResponse = new JSONObject(jsoncommandOutput);
	    LOGGER.info("Json Array retrieved from rfcresponse.json is: " + jsonResponse);
	    // Extract only the parameters from webPA response
	    JSONArray tr181ParametersJson = (new JSONObject(
		    jsonResponse.getString(BroadBandTestConstants.STRING_FEATURECONTROL)))
			    .getJSONArray(BroadBandTestConstants.STRING_FEATURES);
	    LOGGER.info("TR181 PARAMETERS JSON ARRAY:" + tr181ParametersJson);
	    for (counter = 0; counter < tr181ParametersJson.length(); counter++) {
		String configData = tr181ParametersJson.getJSONObject(counter)
			.getString(BroadBandTestConstants.STRING_CONFIG_DATA);
		LOGGER.info("Config Data: " + configData);
		// Fetching only the config data from Json message and updating the config data and its value in map
		// mapper.readValue() returns object, we need hashmap hence type casting
		tr181ParametersMap.putAll((HashMap<String, Object>) mapper.readValue(configData,
			new TypeReference<Map<String, String>>() {
			}));

	    }
	} catch (Exception exception) {
	    LOGGER.error("Exception Occurred while verifying DCM Json Message:" + exception.getMessage());

	}
	LOGGER.debug("Ending Method:extractWebPaParameterAndValueFromJsonFile ");
	return tr181ParametersMap;

    }

    /**
     * Method to extract webpa parameter and value from map and verifying it against log file
     * 
     * @param tapEnv
     *            {@link AutomaticsTapApi} Reference
     * @param device
     *            device
     * @param fileName
     *            fileName used in find command
     *
     * @author Sumathi Gunasekaran
     * @refactor yamini.s
     * 
     */
    public static boolean verifywebPaParameterAndValueInLogFile(AutomaticsTapApi tapEnv, Dut device,
	    Map<String, Object> mapValue, String logFile) {
	LOGGER.debug("Starting Method:verifywebPaParameterAndValueInLogFile");
	Map<String, Object> tr181ParametersMap = new HashMap<String, Object>();
	tr181ParametersMap = mapValue;
	boolean status = false;
	LOGGER.info("Fetching values from Map and verfying in log file");
	String catCommand = BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.CMD_LINUX_CAT,
		BroadBandTestConstants.SINGLE_SPACE_CHARACTER, logFile);
	String response = tapEnv.executeCommandUsingSsh(device, catCommand);
	LOGGER.info("File Contents: \n" + response);

	tr181ParametersMap = extractWebPaParameterAndValueFromRfcFile(response);
	status = !tr181ParametersMap.isEmpty();

	LOGGER.debug("Ending Method:verifywebPaParameterAndValueInLogFile");
	return status;
    }

    /**
     * Method to extract webpa parameter and value from map and verifying it against DCMRFC Log message
     * 
     * @param tapEnv
     *            {@link AutomaticsTapApi} Reference
     * @param device
     *            device
     *
     * @author Sumathi Gunasekaran
     * @refactor yamini.s
     * 
     */
    public static boolean verifywebPaParameterAndValueInDcmRfcLog(AutomaticsTapApi tapEnv, Dut device,
	    Map<String, Object> mapValue) {
	LOGGER.debug("Starting Method:verifywebPaParameterAndValueInDcmRfcLog");
	Map<String, Object> tr181ParametersMap = new HashMap<String, Object>();
	tr181ParametersMap = mapValue;
	String webPAParameter = null;
	String searchCommandForGettingSameValue = null;
	String searchCommandForGettingUpdatedValue = null;
	boolean status = false;
	LOGGER.info("Fetching values from Map and verfying in log file");
	for (String key : tr181ParametersMap.keySet()) {
	    if (!key.equalsIgnoreCase(BroadBandWebPaConstants.WEBPA_PARAM_BAND_STEERING_ENABLE)) {
		webPAParameter = key;

		LOGGER.info("WebPAParameter:" + webPAParameter);
		searchCommandForGettingSameValue = BroadBandCommonUtils.concatStringUsingStringBuffer(
			BroadBandTestConstants.GREP_COMMAND, BroadBandTestConstants.RFC_PARAM,
			BroadBandTestConstants.SINGLE_SPACE_CHARACTER, webPAParameter,
			BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandTestConstants.RFC_SAME_VALUE_MESSAGE,
			BroadBandTestConstants.SINGLE_SPACE_CHARACTER, (String) tr181ParametersMap.get(key),
			BroadBandTestConstants.DOUBLE_QUOTE, BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
			BroadBandCommandConstants.FILE_DCMRFC_LOG);
		LOGGER.info("searchCommandForGettingSameValue is: " + searchCommandForGettingSameValue);
		searchCommandForGettingUpdatedValue = BroadBandCommonUtils.concatStringUsingStringBuffer(
			BroadBandTestConstants.GREP_COMMAND, BroadBandTestConstants.RFC_UPDATED_PARAM,
			BroadBandTestConstants.SINGLE_SPACE_CHARACTER, webPAParameter,
			BroadBandTestConstants.DOUBLE_QUOTE, BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
			BroadBandCommandConstants.FILE_DCMRFC_LOG);
		LOGGER.info("searchCommandForGettingUpdatedValue is: " + searchCommandForGettingUpdatedValue);

		String searchCommandForGettingSetValue = BroadBandCommonUtils.concatStringUsingStringBuffer(
			BroadBandTestConstants.GREP_COMMAND, BroadBandTestConstants.RFC_SET_PARAM, webPAParameter,
			BroadBandTestConstants.DOUBLE_QUOTE, BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
			BroadBandCommandConstants.FILE_DCMRFC_LOG);
		LOGGER.info("searchCommandForGettingSetValue is: " + searchCommandForGettingSetValue);
		status = CommonUtils.searchLogFiles(tapEnv, device, searchCommandForGettingSameValue)
			|| CommonUtils.searchLogFiles(tapEnv, device, searchCommandForGettingUpdatedValue)
			|| CommonUtils.searchLogFiles(tapEnv, device, searchCommandForGettingSetValue);
		if (!status) {
		    return status;
		}
	    }
	}
	LOGGER.debug("Ending Method:verifywebPaParameterAndValueInDcmRfcLog");
	return status;
    }

    /**
     * Method to verify webpa and its response through webpa command
     * 
     * @param tapEnv
     *            {@link AutomaticsTapApi} Reference
     * @param device
     *            device
     * @param fileName
     *            fileName used in find command
     *
     * @author Sumathi Gunasekaran
     * @refactor Rakesh C N
     */
    public static boolean verifywebPaParameterAndValueThroughWebPA(AutomaticsTapApi tapEnv, Dut device,
	    Map<String, Object> mapValue) {
	LOGGER.debug("Starting Method:verifywebPaParameterAndValueThroughWebPA");
	Map<String, Object> tr181ParametersMap = new HashMap<String, Object>();
	tr181ParametersMap = mapValue;
	boolean status = false;
	String webPAParameter = null;
	String webPAResponse = null;
	boolean isAccessPointWebpaParameter = false;
	boolean isWifiRadioWebpaParameter = false;
	String matchValue = null;
	String accessPointValue = null;
	LOGGER.info("Fetching values from Map and verfying in log file");
	for (String key : tr181ParametersMap.keySet()) {

	    if (!key.equalsIgnoreCase(BroadBandWebPaConstants.WEBPA_PARAM_BAND_STEERING_ENABLE)) {
		webPAParameter = key;

		isAccessPointWebpaParameter = CommonMethods.patternMatcher(webPAParameter,
			BroadBandTestConstants.PATTER_MATCHER_FOR_WIFI_ACCESS_POINT);
		isWifiRadioWebpaParameter = CommonMethods.patternMatcher(webPAParameter,
			BroadBandTestConstants.PATTER_MATCHER_FOR_WIFI_RADIO);
		if (isAccessPointWebpaParameter) {
		    matchValue = CommonMethods
			    .patternFinderToReturnAllMatchedString(webPAParameter,
				    BroadBandTestConstants.PATTER_MATCHER_FOR_WIFI_ACCESS_POINT)
			    .get(BroadBandTestConstants.CONSTANT_0);
		    accessPointValue = BroadBandTestConstants.ACCESS_POINT_MAPPING.get(matchValue);
		    webPAParameter = webPAParameter.replaceAll(matchValue, accessPointValue);
		} else if (isWifiRadioWebpaParameter) {
		    matchValue = CommonMethods
			    .patternFinderToReturnAllMatchedString(webPAParameter,
				    BroadBandTestConstants.PATTER_MATCHER_FOR_WIFI_RADIO)
			    .get(BroadBandTestConstants.CONSTANT_0);
		    accessPointValue = BroadBandTestConstants.WIFI_RADIO_MAPPING.get(matchValue);
		    webPAParameter = webPAParameter.replaceAll(matchValue, accessPointValue);
		}
		if (CommonMethods.isNotNull(webPAParameter)) {
		    webPAResponse = tapEnv.executeWebPaCommand(device, webPAParameter).replaceAll("'",
			    BroadBandTestConstants.EMPTY_STRING);
		    LOGGER.info("WebPA Response is:" + webPAResponse);
		    LOGGER.info("The Value to check aganist webPA Response is:" + tr181ParametersMap.get(key));
		    status = webPAResponse.equalsIgnoreCase((String) tr181ParametersMap.get(key));
		    if (!status) {
			return status;
		    }
		}
	    }
	}
	LOGGER.debug("Ending Method:verifywebPaParameterAndValueThroughWebPA");
	return status;
    }

    private static Map<String, Object> extractWebPaParameterAndValueFromRfcFile(String fileContents) {
	Map<String, Object> parameterMap = new HashMap<String, Object>();

	String patternString = "tr181.(.*)#~(.*)#~";
	Pattern pattern = Pattern.compile(patternString, Pattern.CASE_INSENSITIVE);
	Matcher matcher = pattern.matcher(fileContents);
	int count = matcher.groupCount();
	LOGGER.info("Group count: " + count);
	while (matcher.find()) {

	    if (matcher.group(BroadBandTestConstants.CONSTANT_1).trim()
		    .contains(BroadBandTestConstants.STRING_MAX_ASSOCIATED_DEVICES)
		    && BroadBandCommonUtils.convertStringToInteger(matcher.group(BroadBandTestConstants.CONSTANT_2)
			    .trim()) > BroadBandCommonUtils.convertStringToInteger(BroadBandTestConstants.STRING_63)) {
		parameterMap.put(matcher.group(BroadBandTestConstants.CONSTANT_1).trim(),
			BroadBandTestConstants.STRING_63);
	    } else {
		parameterMap.put(matcher.group(BroadBandTestConstants.CONSTANT_1).trim(),
			matcher.group(BroadBandTestConstants.CONSTANT_2).trim());
	    }
	}

	for (Map.Entry<String, Object> entry : parameterMap.entrySet()) {
	    LOGGER.info("Key = " + entry.getKey() + ", Value = " + entry.getValue());
	}
	return parameterMap;
    }

    /**
     * <ol>
     * <li>STEP 1: Search /rdklogs/logs/dcmrfc.log file for http_code: 200</li>
     * <li>STEP 2: Verify if Device.ManagementServer.EnableCWMP is enabled</li>
     * <li>STEP 3: Search for configsethash value in /rdklogs/logs/dcmrfc.log and store it</li>
     * <li>STEP 4: Enable retrieve now parameter</li>
     * <li>STEP 5: Wait for three min and Search for configsethash value in /rdklogs/logs/dcmrfc.log and store it</li>
     * <li>STEP 6: Search /rdklogs/logs/dcmrfc.log file for http_code: 304</li>
     * <li>STEP 7: Post payload data to disable Device.ManagementServer.EnableCWMP feature</li>
     * <li>STEP 8: Wait for three min and Search for http_code: 200 in /rdklogs/logs/dcmrfc.log</li>
     * <li>STEP 9: Verify if Device.ManagementServer.EnableCWMP is disabled</li>
     * <li>STEP 10: Search for configsethash value in /rdklogs/logs/dcmrfc.log and check against previous hash</li>
     * <li>STEP 11: Reboot device, without any change to Xconf</li>
     * </ol>
     * 
     * @refactor Govardhan
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-RFC-1005")
    public void testVerifyRfcStatusCode(Dut device) {

	LOGGER.info("***************************************************************************************");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-RFC-1005");
	LOGGER.info("TEST DESCRIPTION: To verify RFC versioning support ");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("PRE-CONDITION : Post payload data to enable Feature by RFC");
	LOGGER.info("STEP 1: Search /rdklogs/logs/dcmrfc.log file for http_code: 200 ");
	LOGGER.info("STEP 2:  Verify if Device.ManagementServer.EnableCWMP is enabled ");
	LOGGER.info("STEP 3: Search for configsethash value in /rdklogs/logs/dcmrfc.log and store it ");
	LOGGER.info("STEP 4: Enable retrieve now parameter ");
	LOGGER.info(
		"STEP 5: Wait for three min and Search for configsethash value in /rdklogs/logs/dcmrfc.log and store it ");
	LOGGER.info("STEP 6: Search /rdklogs/logs/dcmrfc.log file for http_code: 304 ");
	LOGGER.info("STEP 7: Post payload data to disable Device.ManagementServer.EnableCWMP feature");
	LOGGER.info("STEP 8: Wait for three min and Search for http_code: 200 in /rdklogs/logs/dcmrfc.log ");
	LOGGER.info("STEP 9: Verify if Device.ManagementServer.EnableCWMP is disabled ");
	LOGGER.info(
		"STEP 10: Search for configsethash value in /rdklogs/logs/dcmrfc.log and check against previous hash");
	LOGGER.info("STEP 11: Reboot device, without any change to Xconf ");

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-RFC-105";
	String stepNumber = "s1";
	String errorMessage = null;
	String response = null;
	boolean status = false;
	// hash value after rfc fetch for first time
	String hashValue = null;
	// hash value for second rfc fetch without config changes
	String hashValue2 = null;
	// hash value after rfc config changes
	String hashValue3 = null;
	// Variable Declaration Ends

	try {
	    LOGGER.info("******************* STARTING PRE-CONFIGURATIONS ********************");
	    LOGGER.info("******************************************************************");
	    LOGGER.info("PRE-CONDITION:DESCRIPTION : Post payload data to enable Feature by RFC");
	    LOGGER.info("******************************************************************");
	    LOGGER.info(
		    "PRE-CONDITION : ACTION : Copy dcm.properties and update DCM_RFC_SERVER_URL and reboot the device");
	    LOGGER.info(
		    "PRE-CONDITION : EXPECTED : Device.ManagementServer.EnableCWMP RFC should be Configured in Xconf and update the settings in Device ");
	    errorMessage = "Failed to Enable CWMP through RFC";
	    status = BroadBandRfcFeatureControlUtils.enableOrDisableFeatureByRFC(tapEnv, device,
		    BroadBandTestConstants.CWMP, true);
	    if (!status) {
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + errorMessage);
	    }
	    LOGGER.info("PRE-CONDITION ACTUAL : Fature enabled through RFC Successfully.\n");
	    LOGGER.info("******************* COMPLETED PRE-CONFIGURATIONS ********************");
	    stepNumber = "s1";
	    status = false;
	    LOGGER.info("****************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION: Search /rdklogs/logs/dcmrfc.log file for http_code: 200");
	    LOGGER.info("STEP 1: ACTION: grep -I \"http_code: 200\" /rdklogs/logs/dcmrfc.log");
	    LOGGER.info("STEP 1: EXPECTED: http_code: 200  available in /rdklogs/logs/dcmrfc.log file");
	    LOGGER.info("****************************************************************");
	    errorMessage = "Failed to grep http_code: 200 in /rdklogs/logs/dcmrfc.log file";

	    response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTestConstants.STRING_VALUE_HTTP_CODE_200, BroadBandTestConstants.DCMRFC_LOG_FILE,
		    BroadBandTestConstants.TEN_MINUTE_IN_MILLIS, BroadBandTestConstants.FIVE_SECONDS_IN_MILLIS);
	    status = CommonUtils.isNotEmptyOrNull(response)
		    && (response.contains(BroadBandTestConstants.STRING_VALUE_HTTP_CODE_200));

	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : http_code: 200 is available in /rdklogs/logs/dcmrfc.log");
	    } else {
		LOGGER.error("STEP 1: ACTUAL :" + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
	    // *************************************************************************//
	    boolean tr69Status = BroadBandTr69Utils.checkAndEnableTr69Support(device, tapEnv);
	    LOGGER.info("Status of TR69 before starting the test case : " + tr69Status);

	    stepNumber = "s2";
	    status = false;
	    LOGGER.info("****************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION: Verify if Device.ManagementServer.EnableCWMP is enabled ");
	    LOGGER.info("STEP 2: ACTION: Execute webpa/dmcli to get the value of Device.ManagementServer.EnableCWMP");
	    LOGGER.info("STEP 2: EXPECTED: Device.ManagementServer.EnableCWMP is enabled ");
	    LOGGER.info("****************************************************************");
	    errorMessage = "Unable to verify if cwmp is enabled";

	    status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MANAGEMENTSERVER_ENABLECWMP, BroadBandTestConstants.TRUE,
		    BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.TEN_SECOND_IN_MILLIS);

	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : Device.ManagementServer.EnableCWMP enabled successfully");
	    } else {
		LOGGER.error("STEP 2: ACTUAL :" + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // *************************************************************************//

	    stepNumber = "s3";
	    status = false;
	    LOGGER.info("****************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION: Search for configsethash value in /rdklogs/logs/dcmrfc.log and store it");
	    LOGGER.info("STEP 3: ACTION: Grep -I \"configsethash\" /rdklogs/logs/dcmrfc.log");
	    LOGGER.info("STEP 3: EXPECTED: configsethash is available");
	    LOGGER.info("****************************************************************");
	    errorMessage = "Unable to grep configsethash hash value in /rdklogs/logs/dcmrfc.log file";
	    response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTestConstants.PATTERN_TO_FIND_HASH_VALUE, BroadBandTestConstants.DCMRFC_LOG_FILE,
		    BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.FIVE_SECONDS_IN_MILLIS);
	    if (CommonMethods.isNotNull(response)) {
		hashValue = CommonMethods.patternFinder(response, BroadBandTestConstants.PATTERN_TO_FETCH_HASH_VALUE);

		status = CommonUtils.isNotEmptyOrNull(hashValue);
	    }
	    if (status) {
		LOGGER.info(
			"STEP 3: ACTUAL : configsethash hash value from /rdklogs/logs/dcmrfc.log file stored successfully");
	    } else {
		LOGGER.error("STEP 3: ACTUAL :" + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
	    // *************************************************************************//

	    stepNumber = "s4";
	    status = false;
	    LOGGER.info("****************************************************************");
	    LOGGER.info("STEP 4: DESCRIPTION: Enable retrieve now parameter ");
	    LOGGER.info(
		    "STEP 4: ACTION: Execute webpa set command to set Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Control.RetrieveNow value to 1 ");
	    LOGGER.info(
		    "STEP 4: EXPECTED: Successfully set Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Control.RetrieveNow to 1");
	    LOGGER.info("****************************************************************");
	    errorMessage = "Unable to set retrieve now parameter  to value 1";
	    tapEnv.executeCommandInSettopBox(device, BroadBandCommandConstants.CMD_CLEAR_DCMRFC_LOG);

	    status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    WebPaParamConstants.WEBPA_PARAM_IMMEDIATE_RFC_CHECK_IN, BroadBandTestConstants.CONSTANT_2,
		    BroadBandTestConstants.STRING_VALUE_ONE);

	    if (status) {
		LOGGER.info("STEP 4: ACTUAL : Successfully set retrieve now to value 1");
	    } else {
		LOGGER.error("STEP 4: ACTUAL :" + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
	    // *************************************************************************//

	    stepNumber = "s5";
	    status = false;
	    LOGGER.info("****************************************************************");
	    LOGGER.info(
		    "STEP 5: DESCRIPTION: Wait for three min and Search for configsethash value in /rdklogs/logs/dcmrfc.log and store it");
	    LOGGER.info("STEP 5: ACTION: execute grep -I \"configsethash\" /rdklogs/logs/dcmrfc.log ");
	    LOGGER.info("STEP 5: EXPECTED: configsethash is available and matches with first config hash value");
	    LOGGER.info("****************************************************************");

	    errorMessage = "Failed to search for configsethash log message ";
	    tapEnv.waitTill(BroadBandTestConstants.THREE_MINUTE_IN_MILLIS);
	    response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTestConstants.PATTERN_TO_FIND_HASH_VALUE, BroadBandTestConstants.DCMRFC_LOG_FILE,
		    BroadBandTestConstants.TEN_MINUTE_IN_MILLIS, BroadBandTestConstants.FIVE_SECONDS_IN_MILLIS);
	    if (CommonMethods.isNotNull(response)) {
		errorMessage = "Failed to fetch hash key from log after retrieve now set to true";
		hashValue2 = CommonMethods.patternFinder(response, BroadBandTestConstants.PATTERN_TO_FETCH_HASH_VALUE);
		LOGGER.info("After retreive now fetch hash value is " + hashValue2);
		status = CommonUtils.isNotEmptyOrNull(hashValue2) && hashValue2.equalsIgnoreCase(hashValue);
	    }
	    if (status) {
		LOGGER.info("STEP 5: ACTUAL : Successfully verified hash value against previous value");
	    } else {
		LOGGER.error("STEP 5: ACTUAL :" + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
	    // *************************************************************************//

	    stepNumber = "s6";
	    status = false;
	    LOGGER.info("****************************************************************");
	    LOGGER.info("STEP 6: DESCRIPTION: Search /rdklogs/logs/dcmrfc.log file for http_code: 304");
	    LOGGER.info("STEP 6: ACTION: grep -I \"http_code: 304\" /rdklogs/logs/dcmrfc.log");
	    LOGGER.info("STEP 6: EXPECTED: http_code: 304  available in /rdklogs/logs/dcmrfc.log file");
	    LOGGER.info("****************************************************************");
	    errorMessage = "Failed to Enable CWMP through RFC";
	    response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTestConstants.HTTP_RESPONSE_CODE_304, BroadBandTestConstants.DCMRFC_LOG_FILE,
		    BroadBandTestConstants.TEN_MINUTE_IN_MILLIS, BroadBandTestConstants.FIVE_SECONDS_IN_MILLIS);

	    status = CommonUtils.isNotEmptyOrNull(response)
		    && response.contains(BroadBandTestConstants.HTTP_RESPONSE_CODE_304);

	    if (status) {
		LOGGER.info("STEP 6: ACTUAL : Successfully verfied log message http_code: 304");
	    } else {
		LOGGER.error("STEP 6: ACTUAL :" + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // *************************************************************************//

	    stepNumber = "s7";
	    status = false;
	    LOGGER.info("****************************************************************");
	    LOGGER.info("STEP 7: DESCRIPTION: Post payload data to disable Device.ManagementServer.EnableCWMP feature");
	    LOGGER.info("STEP 7: ACTION: Copy dcm.properties and update DCM_RFC_SERVER_URL and reboot the device");
	    LOGGER.info(
		    "STEP 7: EXPECTED: Device.ManagementServer.EnableCWMP RFC should be Configured in Xconf and update the settings in Device");
	    LOGGER.info("****************************************************************");
	    errorMessage = "Failed to disable CWMP through RFC";
	    status = BroadBandRfcFeatureControlUtils.executePreconditionForRfcTests(device, tapEnv,
		    AutomaticsTapApi.getSTBPropsValue(BroadBandTestConstants.PROP_KEY_PAYLOAD_CWMP_DISABLE));

	    if (status) {
		errorMessage = "Failed to set retrieve now to value 1 ";
		status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			WebPaParamConstants.WEBPA_PARAM_IMMEDIATE_RFC_CHECK_IN, BroadBandTestConstants.CONSTANT_2,
			BroadBandTestConstants.STRING_VALUE_ONE);

	    }
	    if (status) {
		LOGGER.info("STEP 7: ACTUAL : Successfully disbaled CWMP through RFC");
	    } else {
		LOGGER.error("STEP 7: ACTUAL :" + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
	    // *************************************************************************//
	    stepNumber = "s8";
	    status = false;
	    LOGGER.info("****************************************************************");
	    LOGGER.info(
		    "STEP 8: DESCRIPTION: Wait for three min and Search for http_code: 200 in /rdklogs/logs/dcmrfc.log");
	    LOGGER.info("STEP 8: ACTION: grep -I \"http_code: 200\" /rdklogs/logs/dcmrfc.log");
	    LOGGER.info("STEP 8: EXPECTED: http_code: 200  available in /rdklogs/logs/dcmrfc.log file");
	    LOGGER.info("****************************************************************");

	    errorMessage = "Failed to verify status code 200";
	    response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTestConstants.STRING_VALUE_HTTP_CODE_200, BroadBandTestConstants.DCMRFC_LOG_FILE,
		    BroadBandTestConstants.TEN_MINUTE_IN_MILLIS, BroadBandTestConstants.FIVE_SECONDS_IN_MILLIS);
	    status = CommonUtils.isNotEmptyOrNull(response);

	    if (status) {
		LOGGER.info("STEP 8: ACTUAL : http_code: 200 is available in /rdklogs/logs/dcmrfc.log");
	    } else {
		LOGGER.error("STEP 8: ACTUAL :" + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // *************************************************************************//

	    // *************************************************************************//
	    stepNumber = "s9";
	    status = false;
	    LOGGER.info("****************************************************************");
	    LOGGER.info("STEP 9: DESCRIPTION: Verify if Device.ManagementServer.EnableCWMP is disabled ");
	    LOGGER.info("STEP 9: ACTION: Execute webpa/dmcli to get the value of Device.ManagementServer.EnableCWMP");
	    LOGGER.info("STEP 9: EXPECTED: Device.ManagementServer.EnableCWMP is disbaled ");
	    LOGGER.info("****************************************************************");
	    errorMessage = "failed to verify if Device.ManagementServer.EnableCWMP is disabled";
	    status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MANAGEMENTSERVER_ENABLECWMP,
		    BroadBandTestConstants.FALSE, BroadBandTestConstants.TEN_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.TEN_SECOND_IN_MILLIS);

	    if (status) {
		LOGGER.info("STEP 9: ACTUAL : Device.ManagementServer.EnableCWMP disabled successfully ");
	    } else {
		LOGGER.error("STEP 9: ACTUAL :" + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // *************************************************************************//
	    stepNumber = "s10";
	    status = false;
	    LOGGER.info("****************************************************************");
	    LOGGER.info(
		    "STEP 10: DESCRIPTION: Search for configsethash value in /rdklogs/logs/dcmrfc.log and check against previous hash");
	    LOGGER.info("STEP 10: ACTION: execute grep -I \"configsethash\" /rdklogs/logs/dcmrfc.log ");
	    LOGGER.info("STEP 10: EXPECTED: configsethash is available ");
	    LOGGER.info("****************************************************************");

	    errorMessage = "Unbale to retrieve hash value";
	    tapEnv.waitTill(BroadBandTestConstants.THREE_MINUTE_IN_MILLIS);
	    response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTestConstants.PATTERN_TO_FIND_HASH_VALUE, BroadBandTestConstants.DCMRFC_LOG_FILE,
		    BroadBandTestConstants.TEN_MINUTE_IN_MILLIS, BroadBandTestConstants.FIVE_SECONDS_IN_MILLIS);
	    if (CommonMethods.isNotNull(response)) {
		hashValue3 = CommonMethods.patternFinder(response, BroadBandTestConstants.PATTERN_TO_FETCH_HASH_VALUE);

		status = CommonUtils.isNotEmptyOrNull(hashValue3) && (!hashValue2.equalsIgnoreCase(hashValue3));
	    }
	    if (status) {
		LOGGER.info("STEP 10: ACTUAL : Successfully verified hash value against previous value ");
	    } else {
		LOGGER.error("STEP 10: ACTUAL :" + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // *************************************************************************//

	    stepNumber = "s11";
	    status = false;
	    LOGGER.info("****************************************************************");
	    LOGGER.info(
		    "STEP 11: DESCRIPTION: Reboot device, without any change to Xconf and check hash value against previous hash value");
	    LOGGER.info("STEP 11: ACTION: execute grep -I \"configsethash\" /rdklogs/logs/dcmrfc.log ");
	    LOGGER.info(
		    "STEP 11: EXPECTED: Data post is successful with http 200 response, but the configsethash is the same as on in previous as expected  . ");
	    LOGGER.info("****************************************************************");
	    errorMessage = "failed to reboot the device";

	    if (CommonUtils.rebootAndWaitForIpAcquisition(tapEnv, device)) {
		errorMessage = "Failed to verify status code 200";
		response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			BroadBandTestConstants.STRING_VALUE_HTTP_CODE_200, BroadBandTestConstants.DCMRFC_LOG_FILE,
			BroadBandTestConstants.TEN_MINUTE_IN_MILLIS, BroadBandTestConstants.FIVE_SECONDS_IN_MILLIS);
		if (CommonUtils.isNotEmptyOrNull(response)) {
		    errorMessage = "Unable to retrieve hash value";
		    response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			    BroadBandTestConstants.PATTERN_TO_FIND_HASH_VALUE, BroadBandTestConstants.DCMRFC_LOG_FILE,
			    BroadBandTestConstants.TEN_MINUTE_IN_MILLIS, BroadBandTestConstants.FIVE_SECONDS_IN_MILLIS);
		    if (CommonUtils.isNotEmptyOrNull(response)) {
			errorMessage = " Unable to verify hash  value against previous hash  key";
			String after_reboot = CommonMethods.patternFinder(response,
				BroadBandTestConstants.PATTERN_TO_FETCH_HASH_VALUE);
			status = CommonUtils.isNotEmptyOrNull(after_reboot)
				&& after_reboot.equalsIgnoreCase(hashValue3);
		    }
		} else {
		    LOGGER.error(errorMessage);
		}
	    } else {
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info(
			"STEP 11: ACTUAL : Successfully verified hash value after the reboot without xconf change ");
	    } else {
		LOGGER.error("STEP 11: ACTUAL :" + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // *************************************************************************//

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
		    true);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");
	    LOGGER.info(
		    "POST-CONDITION : DESCRIPTION : Remove nvram override of rfc.properties and disable tr181 parameter.");
	    LOGGER.info("POST-CONDITION : ACTION : Remove /nvram/rfc.properties and execute webpa or dmcli command to"
		    + " set value of Device.ManagementServer.EnableCWMP to false");
	    LOGGER.info("POST-CONDITION : EXPECTED : WebPA set is successful and parmeter value is false");

	    status = CommonUtils.removeFileandVerifyFileRemoval(tapEnv, device,
		    BroadBandRfcFeatureControlUtils.NVRAM_RFC_PROPERTIES)
		    && BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MANAGEMENTSERVER_ENABLECWMP,
			    BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.FALSE);

	    if (status) {
		LOGGER.info("POST-CONDITION : ACTUAL : Post condition executed successfully");
	    } else {
		LOGGER.error("POST-CONDITION : ACTUAL : Post condition failed");
	    }
	    LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");

	}
    }

    /**
     * Verify Reboot required RFC change causes scheduled reboot in maintenance window
     * <ol>
     * <li>Get current value of Encrypt Cloud Upload Enable parameter</li>
     * <li>Configure RFC payload data to enable/disable Encrypt Cloud Upload based on current value</li>
     * <li>Set custom maintenance window based on device date</li>
     * <li>Set RFC RetrieveNow parameter to uint 1 to fetch config</li>
     * <li>Verify Encrypt Cloud Upload Enable parameter is updated based on RFC config</li>
     * <li>Verify log message RFC cron scheduled</li>
     * <li>Verify crontab has RFC_Reboot.sh scheduled</li>
     * <li>Verify device goes for a reboot in maintenance window</li>
     * <li>Verify reboot pending notification is logged for this reboot</li>
     * <li>Verify last reboot reason for RFC reboot</li>
     * </ol>
     * 
     * @author Ashwin Sankara
     * @refactor Govardhan
     * 
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-RFC-1006")
    public void testVerifyRfcWindowReboot(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-RFC-106";
	String stepNum = "s1";
	String errorMessage = null;
	String defaultValue = null;
	boolean status = false;
	BroadBandResultObject result = new BroadBandResultObject();
	long waitTime = BroadBandTestConstants.CONSTANT_7;
	long presentTimeForIP = 0;
	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-RFC-1006");
	LOGGER.info(
		"TEST DESCRIPTION: Verify Reboot required RFC change causes scheduled reboot in maintenance window");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Get current value of Encrypt Cloud Upload Enable parameter");
	LOGGER.info("2. Configure RFC payload data to enable/disable Encrypt Cloud  Upload based on current value");
	LOGGER.info("3. Set custom maintenance window based on device date");
	LOGGER.info("4. Set RFC RetrieveNow parameter to uint 1 to fetch config");
	LOGGER.info("5. Verify Encrypt Cloud Upload Enable parameter is updated based on RFC config");
	LOGGER.info("6. Verify log message RFC cron scheduled");
	LOGGER.info("7. Verify crontab has RFC_Reboot.sh scheduled");
	LOGGER.info("8. Verify device goes for a reboot in maintenance window");
	LOGGER.info("9. Verify reboot pending notification is logged for this reboot");
	LOGGER.info("10. Verify last reboot reason for RFC reboot");

	LOGGER.info("#######################################################################################");
	try {
	    stepNum = "s1";
	    errorMessage = "Failed to get value of Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.EncryptCloudUpload.Enable";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Get current value of Encrypt Cloud Upload Enable parameter");
	    LOGGER.info(
		    "STEP 1: ACTION : Execute command:dmcli eRT getv Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.EncryptCloudUpload.Enable");
	    LOGGER.info("STEP 1: EXPECTED : Value is stored as true or false");
	    LOGGER.info("**********************************************************************************");
	    defaultValue = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.TR69_PARAM_DEVICE_RFC_FEATURE_ENCRYPT_CLOUD_UPLOAD_ENABLE);
	    status = CommonMethods.isNotNull(defaultValue);
	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : Value is stored as true or false");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "s2";
	    errorMessage = "Failed to configure RFC payload data for Encrypt Cloud Upload enable or disable";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 2: DESCRIPTION : Configure RFC payload data to enable/disable Encrypt Cloud  Upload based on current value");
	    LOGGER.info(
		    "STEP 2: ACTION : 1. Post following payload:{\"estbMacAddress\":\"ESTB_MAC_ADDRESS\",\"features\":[{\"name\":\"EncryptCloudData\",\"effectiveImmediate\":true,\"enable\":true,\"configData\":{\"tr181.Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.EncryptCloudUpload.Enable\":\"<opposite_value_from_s1>\"}}]}\n2. copy and update rfc.properties with mock url");
	    LOGGER.info("STEP 2: EXPECTED : Configured RFC payload data to enable/disable Encrypt Cloud Upload");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandRfcFeatureControlUtils.executePreconditionForRfcTests(device, tapEnv,
		    AutomaticsTapApi
			    .getSTBPropsValue(BroadBandTestConstants.PROP_KEY_TO_ENABLE_DISABLE_CLOUD_UPLOAD_ENCRYPTION)
			    .replace(BroadBandTestConstants.STRING_TO_CHANGE_ENCRYPT_CLOUD_UPLOAD_STATUS,
				    (BroadBandTestConstants.TRUE.equalsIgnoreCase(defaultValue)
					    ? BroadBandTestConstants.FALSE
					    : BroadBandTestConstants.TRUE)));
	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : Configured RFC payload data to enable/disable Encrypt Cloud Upload");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "s3";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION : Set custom maintenance window based on device date");
	    LOGGER.info(
		    "STEP 3: ACTION : Execute commands:1. date -d \"00:00:00\" \"+%s\"2. date +%s3. Get value of Device.Time.TimeOffset3. Set Device.DeviceInfo.X_RDKCENTRAL-COM_MaintenanceWindow.FirmwareUpgradeStartTime to \"value of (2) - value of (1) + value of (3) + 180\"4. Set Device.DeviceInfo.X_RDKCENTRAL-COM_MaintenanceWindow.FirmwareUpgradeEndTime to FirmwareUpgradeStartTime plus 9005. Verify values have been set successfully");
	    LOGGER.info(
		    "STEP 3: EXPECTED : Set custom maintenance window for 15 minutes from current time successfully");
	    LOGGER.info("**********************************************************************************");
	    try {
		maintenanceWindow = BroadBandCommonUtils
			.calculateFirmwareUpgradeMaintenanceWindowWithTimeInterval(tapEnv, device, waitTime);
		status = BroadBandCommonUtils.calculateAndSetMaintanenceWindow(device, tapEnv, waitTime,
			maintenanceWindow);
	    } catch (Exception e) {
		status = false;
		errorMessage += "Exception occured while setting the maintenance window ." + e.getMessage();
	    }
	    if (status) {
		LOGGER.info(
			"STEP 3: ACTUAL : Set custom maintenance window for 15 minutes from current time successfully");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "s4";
	    errorMessage = "Failed to set RetrieveNow parameter to uint 1 successfully";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 4: DESCRIPTION : Set RFC RetrieveNow parameter to uint 1 to fetch config");
	    LOGGER.info(
		    "STEP 4: ACTION : Execute command:dmcli eRT setv Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Control.RetrieveNow uint 1");
	    LOGGER.info("STEP 4: EXPECTED : Successfully set RFC RetrieveNow parameter");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    WebPaParamConstants.WEBPA_PARAM_IMMEDIATE_RFC_CHECK_IN, BroadBandTestConstants.CONSTANT_2,
		    BroadBandTestConstants.STRING_VALUE_ONE);
	    if (status) {
		LOGGER.info("STEP 4: ACTUAL : Successfully set RFC RetrieveNow parameter");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "s5";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 5: DESCRIPTION : Verify Encrypt Cloud Upload Enable parameter is updated based on RFC config");
	    LOGGER.info(
		    "STEP 5: ACTION : Execute command:dmcli eRT getv Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.EncryptCloudUpload.Enable");
	    LOGGER.info("STEP 5: EXPECTED : Value is updated based on RFC config");
	    LOGGER.info("**********************************************************************************");
	    result = BroadBandRfcFeatureControlUtils.verifyParameterUpdatedByRfc(device, tapEnv,
		    BroadBandWebPaConstants.TR69_PARAM_DEVICE_RFC_FEATURE_ENCRYPT_CLOUD_UPLOAD_ENABLE,
		    (BroadBandTestConstants.TRUE.equalsIgnoreCase(defaultValue) ? BroadBandTestConstants.FALSE
			    : BroadBandTestConstants.TRUE));
	    errorMessage = result.getErrorMessage();
	    status = result.isStatus();
	    if (status) {
		LOGGER.info("STEP 5: ACTUAL : Value is updated based on RFC config");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "s6";
	    errorMessage = "Failed to find log message for RFC cron scheduled when reboot required is true";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 6: DESCRIPTION : Verify log message RFC cron scheduled");
	    LOGGER.info(
		    "STEP 6: ACTION : Execute command:grep -i \"RFC Reboot cron job scheduled\" /rdklogs/logs/dcmrfc.log");
	    LOGGER.info("STEP 6: EXPECTED : Log message is present in dcmrfc.log");
	    LOGGER.info("**********************************************************************************");
	    status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTraceConstants.LOG_MESSAGE_RFC_REBOOT_CRON_SCHEDULED,
		    BroadBandCommandConstants.FILE_DCMRFC_LOG, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS));
	    if (status) {
		LOGGER.info("STEP 6: ACTUAL : Log message is present in dcmrfc.log");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "s7";
	    errorMessage = "Failed to find RFC_Reboot.sh present in crontab";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 7: DESCRIPTION : Verify crontab has RFC_Reboot.sh scheduled");
	    LOGGER.info("STEP 7: ACTION : Execute command:crontab -l");
	    LOGGER.info("STEP 7: EXPECTED : RFC_Reboot.sh is present in crontab");
	    LOGGER.info("**********************************************************************************");
	    status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTestConstants.RFC_REBOOT_SH, BroadBandCommandConstants.ROOT_CRON_TAB));
	    if (status) {
		LOGGER.info("STEP 7: ACTUAL : RFC_Reboot.sh is present in crontab");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "s8";
	    errorMessage = "Device failed to reboot in maintenance window";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 8: DESCRIPTION : Verify device goes for a reboot in maintenance window");
	    LOGGER.info(
		    "STEP 8: ACTION : Execute command:tail -f /rdklogs/logs/PARODUSlog > /nvram/PARODUStail.txt, Wait for device to reboot and come up within 15 mins");
	    LOGGER.info("STEP 8: EXPECTED : Device rebooted successfully in maintenance window");
	    LOGGER.info("**********************************************************************************");
	    tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_GET_PARODUSLOGS_NVRAM);
	    status = BroadBandCommonUtils.verifyDeviceRebootDuringMaintenanceWindow(device, tapEnv, maintenanceWindow);
	    long startTime = System.currentTimeMillis();
	    if (status) {
		do {
		    status = CommonMethods.isSTBAccessible(device);
		} while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.TEN_MINUTE_IN_MILLIS
			&& !status && BroadBandCommonUtils.hasWaitForDuration(tapEnv,
				BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    }
	    if (status) {
		LOGGER.info("STEP 8: ACTUAL : Device rebooted successfully in maintenance window and came up online");
	    } else {
		LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "s9";
	    if (DeviceModeHandler.isBusinessClassDevice(device)) {
		LOGGER.info("STEP 9 is not applicable for BCI devices as they do not send reboot notification to xFi");
		tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
			"BCI devices do not send reboot notification to xFi", false);
	    } else {
		errorMessage = "Failed to find reboot-pending notification present for RFC reboot";
		status = false;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP 9: DESCRIPTION : Verify reboot pending notification is logged for this reboot");
		LOGGER.info("STEP 9: ACTION : Execute command:grep -i \"reboot-pending\" /nvram/PARODUStail.txt");
		LOGGER.info("STEP 9: EXPECTED : Reboot pending notification is present");
		LOGGER.info("**********************************************************************************");
		status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			BroadBandTraceConstants.LOG_MESSAGE_REBOOT_PENDING_NOTIFICATION,
			BroadBandCommandConstants.FILE_PATH_NVRAM_PARODUS_TAIL));
		if (status) {
		    LOGGER.info("STEP 9: ACTUAL : Reboot pending notification is present");
		} else {
		    LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    }

	    stepNum = "s10";
	    errorMessage = "Failed to verify last reboot reason as rfc_reboot";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 10: DESCRIPTION : Verify last reboot reason for RFC reboot");
	    LOGGER.info(
		    "STEP 10: ACTION : Execute command:dmcli eRT getv Device.DeviceInfo.X_RDKCENTRAL-COM_LastRebootReason");
	    LOGGER.info("STEP 10: EXPECTED : Last reboot reason is rfc_reboot");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_LAST_REBOOT_REASON,
		    BroadBandTestConstants.REBOOT_REASON_RFC_REBOOT, BroadBandTestConstants.TEN_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    if (status) {
		LOGGER.info("STEP 10: ACTUAL : Last reboot reason is rfc_reboot");
	    } else {
		LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	} finally {
	    if (CommonMethods.isNotNull(defaultValue)) {
		LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
		LOGGER.info("POST-CONDITION STEPS");
		LOGGER.info("POST-CONDITION 1: DESCRIPTION : Reset value of Encrypt Cloud Upload Enable parameter");
		LOGGER.info(
			"POST-CONDITION 1: ACTION : dmcli eRT setv Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.EncryptCloudUpload.Enable bool <default_value>");
		LOGGER.info("POST-CONDITION 1: EXPECTED : Value set successfully");

		status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			WebPaParamConstants.WEBPA_PARAM_TO_ENABLE_DISABLE_CLOUD_UPLOAD_ENCRYPTION,
			BroadBandTestConstants.CONSTANT_3, defaultValue);

		if (status) {
		    LOGGER.info("POST-CONDITION 1: ACTUAL : Post condition executed successfully");
		} else {
		    LOGGER.error("POST-CONDITION 1: ACTUAL : Post condition failed");
		}

		LOGGER.info("**********************************************************************************");
		LOGGER.info("POST-CONDITION 2: DESCRIPTION : Remove test file from nvram");
		LOGGER.info("POST-CONDITION 2: ACTION : Execute command: rm -rf /nvram/PARODUStail.txt");
		LOGGER.info("POST-CONDITION 2: EXPECTED : File removed successfully");

		status = CommonUtils.removeFileandVerifyFileRemoval(tapEnv, device,
			BroadBandCommandConstants.FILE_PATH_NVRAM_PARODUS_TAIL);

		if (status) {
		    LOGGER.info("POST-CONDITION 2: ACTUAL : Post condition executed successfully");
		} else {
		    LOGGER.error("POST-CONDITION 2: ACTUAL : Post condition failed");
		}

		LOGGER.info("**********************************************************************************");
		LOGGER.info("POST-CONDITION 3: DESCRIPTION : Reset FirmwareUpgradeStartTime to default");
		LOGGER.info(
			"POST-CONDITION 3: ACTION : Execute command: dmcli eRT setv Device.DeviceInfo.X_RDKCENTRAL-COM_MaintenanceWindow.FirmwareUpgradeStartTime string 0");
		LOGGER.info("POST-CONDITION 3: EXPECTED : Value set successfully");

		status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_COMMAND_MAINTENANCE_WINDOW_START_TIME,
			BroadBandTestConstants.CONSTANT_0, BroadBandTestConstants.STRING_ZERO);

		if (status) {
		    LOGGER.info("POST-CONDITION 3: ACTUAL : Post condition executed successfully");
		} else {
		    LOGGER.error("POST-CONDITION 3: ACTUAL : Post condition failed");
		}

		LOGGER.info("**********************************************************************************");
		LOGGER.info("POST-CONDITION 4: DESCRIPTION : Reset FirmwareUpgradeEndTime to default");
		LOGGER.info(
			"POST-CONDITION 4: ACTION : Execute command:dmcli eRT setv Device.DeviceInfo.X_RDKCENTRAL-COM_MaintenanceWindow.FirmwareUpgradeEndTime string 10800");
		LOGGER.info("POST-CONDITION 4: EXPECTED : Value set successfully");

		status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_COMMAND_MAINTENANCE_WINDOW_START_TIME,
			BroadBandTestConstants.CONSTANT_0, BroadBandTestConstants.STRING_VALUE_10800);

		if (status) {
		    LOGGER.info("POST-CONDITION 4: ACTUAL : Post condition executed successfully");
		} else {
		    LOGGER.error("POST-CONDITION 4: ACTUAL : Post condition failed");
		}

		LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	    }
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-RFC-1006");
    }

    /**
     * Verify RFC client uses get-test-set for TR181 parameters
     * <ol>
     * <li>Clear dcmrfc.log and trigger Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Control.RetrieveNow</li>
     * <li>Verify RFC processing log messages present in dcmrfc.log for posted parameter value</li>
     * <li>Verify old parameter value is logged in dcmrfc.log for the posted parameter</li>
     * <li>Verify new posted parameter value is logged in dcmrfc.log</li>
     * <li>Verify RFC updated parameter value from old to new message is present in dcmrfc.log</li>
     * <li>Verify parameter value is true after being updated by RFC</li>
     * <li>Clear dcmrfc.log and trigger Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Control.RetrieveNow</li>
     * <li>Verify old parameter value is logged in dcmrfc.log for the posted parameter</li>
     * <li>Verify new posted parameter value is logged in dcmrfc.log</li>
     * <li>Verify parameter old and new values are same log message is present in dcmrfc.log</li>
     * <li>Verify parameter value is true after RFC processing</li>
     * </ol>
     *
     * @param device
     *            {@link Dut}
     * 
     * @author Ashwin Sankara
     * @refactor Govardhan
     * 
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-RFC-1003")
    public void testVerifyRfcGetTestSet(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-RFC-103";
	String stepNum = "s1";
	String errorMessage = null;
	String response = null;
	boolean status = false;
	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-RFC-1003");
	LOGGER.info(
		"TEST DESCRIPTION: Verify RFC processing log messages present in dcmrfc.log for posted parameter value");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Clear dcmrfc.log and trigger Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Control.RetrieveNow");
	LOGGER.info("2. Verify RFC processing log messages present in dcmrfc.log for posted parameter value");
	LOGGER.info("3. Verify old parameter value is logged in dcmrfc.log for the posted parameter");
	LOGGER.info("4. Verify new posted parameter value is logged in dcmrfc.log");
	LOGGER.info("5. Verify RFC updated parameter value from old to new message is present in dcmrfc.log");
	LOGGER.info("6. Verify parameter value is true after being updated by RFC");
	LOGGER.info("7. Clear dcmrfc.log and trigger Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Control.RetrieveNow");
	LOGGER.info("9. Verify old parameter value is logged in dcmrfc.log for the posted parameter");
	LOGGER.info("10. Verify new posted parameter value is logged in dcmrfc.log");
	LOGGER.info("11. Verify parameter old and new values are same log message is present in dcmrfc.log");
	LOGGER.info("12. Verify parameter value is true after RFC processing");

	LOGGER.info("#######################################################################################");

	try {

	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    LOGGER.info("PRE-CONDITION : DESCRIPTION : Configure RFC payload data to enable tr181 parameter when"
		    + " RFC retrieve now is triggered. Verify Encrypt Cloud Upload parameter is disabled, else disable it");
	    LOGGER.info("PRE-CONDITION : ACTION : Copy update rfc.properties, post payload,"
		    + " verify parameter disabled, else disable it");
	    LOGGER.info("PRE-CONDITION : EXPECTED : Data post is successful and parameter value is false");

	    errorMessage = "Unable to configure RFC payload to enable Encrypt Cloud Upload";
	    if (BroadBandRfcFeatureControlUtils.executePreconditionForRfcTests(device, tapEnv,
		    AutomaticsTapApi
			    .getSTBPropsValue(BroadBandTestConstants.PROP_KEY_TO_ENABLE_DISABLE_CLOUD_UPLOAD_ENCRYPTION)
			    .replace(BroadBandTestConstants.STRING_TO_CHANGE_ENCRYPT_CLOUD_UPLOAD_STATUS,
				    BroadBandTestConstants.TRUE))) {
		errorMessage = "Unable to set Encrypt Cloud Upload Enable parameter to false";
		status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
			WebPaParamConstants.WEBPA_PARAM_TO_ENABLE_DISABLE_CLOUD_UPLOAD_ENCRYPTION,
			BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.FALSE,
			BroadBandTestConstants.CONSTANT_0, BroadBandTestConstants.CONSTANT_0);
	    }

	    if (status) {
		LOGGER.info("PRE-CONDITION : ACTUAL : Pre condition executed successfully");
	    } else {
		LOGGER.error("PRE-CONDITION : ACTUAL : Pre condition failed");
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + errorMessage);
	    }

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s1";
	    errorMessage = "Unable to set RFC RetrieveNow parameter with unsigned int value 1";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 1: DESCRIPTION : Clear dcmrfc.log and trigger Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Control.RetrieveNow");
	    LOGGER.info("STEP 1: ACTION : Remove dcmrfc.log and execute webpa or dmcli command to set "
		    + "Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Control.RetrieveNow to unsigned int value 1");
	    LOGGER.info("STEP 1: EXPECTED : Successfully set RFC.RetrieveNow parameter with unsigned int 1");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    WebPaParamConstants.WEBPA_PARAM_IMMEDIATE_RFC_CHECK_IN, BroadBandTestConstants.CONSTANT_2,
		    BroadBandTestConstants.STRING_VALUE_ONE);

	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : Successfully set RFC.RetrieveNow parameter with unsigned int 1");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s2";
	    errorMessage = "Unable to find log message with posted parameter value in dcmrfc.log";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 2: DESCRIPTION : Verify RFC processing log messages present in dcmrfc.log for posted parameter value");
	    LOGGER.info("STEP 2: ACTION : Execute command: grep -A 5 -i \"key=tr181.Device.DeviceInfo."
		    + "X_RDKCENTRAL-COM_RFC.Feature.EncryptCloudUpload.Enable value=true\" /rdklogs/logs/dcmrfc.log");
	    LOGGER.info("STEP 2: EXPECTED : Log message with posted tr181 parameter and value is present");
	    LOGGER.info("**********************************************************************************");

	    response = BroadBandRfcFeatureControlUtils.searchFeatureLogsInDcmRfcLog(device, tapEnv,
		    WebPaParamConstants.WEBPA_PARAM_TO_ENABLE_DISABLE_CLOUD_UPLOAD_ENCRYPTION,
		    BroadBandTestConstants.TRUE);
	    status = CommonMethods.isNotNull(response);

	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : Log message with posted tr181 parameter and value is present");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s3";
	    errorMessage = "Unable to find old parameter value false logged in dcmrfc.log for parameter processed by RFC";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 3: DESCRIPTION : Verify old parameter value is logged in dcmrfc.log for the posted parameter");
	    LOGGER.info("STEP 3: ACTION : Verify output of step 2 contains: RFC: old parameter value false");
	    LOGGER.info("STEP 3: EXPECTED : Old parameter value log message is present in output");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.patternMatcher(response, BroadBandCommonUtils.concatStringUsingStringBuffer(
		    BroadBandTraceConstants.LOG_MESSAGE_OLD_PARAM_VALUE, BroadBandTestConstants.FALSE));

	    if (status) {
		LOGGER.info("STEP 3: ACTUAL : Old parameter value log message is present in output");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s4";
	    errorMessage = "Unable to find new parameter value true logged in dcmrfc.log for parameter processed by RFC";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 4: DESCRIPTION : Verify new posted parameter value is logged in dcmrfc.log");
	    LOGGER.info("STEP 4: ACTION : Verify output of step 2 contains: Parameter value  true");
	    LOGGER.info("STEP 4: EXPECTED : New parameter value log message is present in output");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.patternMatcher(response, BroadBandCommonUtils.concatStringUsingStringBuffer(
		    BroadBandTraceConstants.LOG_MESSAGE_NEW_PARAM_VALUE, BroadBandTestConstants.TRUE));

	    if (status) {
		LOGGER.info("STEP 4: ACTUAL : New parameter value log message is present in output");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s5";
	    errorMessage = "Unable to find parameter value updated log message in dcmrfc.log for parameter updated by RFC";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 5: DESCRIPTION : Verify RFC updated parameter value from old to new message is present in dcmrfc.log");
	    LOGGER.info("STEP 5: ACTION : Verify output of step 2 contains: RFC:  updated for Device.DeviceInfo."
		    + "X_RDKCENTRAL-COM_RFC.Feature.EncryptCloudUpload.Enable from value old=false, to new=true");
	    LOGGER.info("STEP 5: EXPECTED : RFC updated parameter value from old to new  message is present in output");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.patternMatcher(response, BroadBandTraceConstants.LOG_MESSAGE_PARAM_VALUE_UPDATED);

	    if (status) {
		LOGGER.info(
			"STEP 5: ACTUAL : RFC updated parameter value from old to new  message is present in output");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s6";
	    errorMessage = "Value of parameter is not true after being updated by RFC";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 6: DESCRIPTION : Verify parameter value is true after being updated by RFC");
	    LOGGER.info("STEP 6: ACTION : Execute webpa or dmcli command to get value of"
		    + " Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.EncryptCloudUpload.Enable");
	    LOGGER.info("STEP 6: EXPECTED : Value of  EncryptCloudUpload Enable parameter value is true");
	    LOGGER.info("**********************************************************************************");

	    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    WebPaParamConstants.WEBPA_PARAM_TO_ENABLE_DISABLE_CLOUD_UPLOAD_ENCRYPTION);
	    status = CommonMethods.isNotNull(response) && response.equals(BroadBandTestConstants.TRUE);

	    if (status) {
		LOGGER.info("STEP 6: ACTUAL : Value of  EncryptCloudUpload Enable parameter value is true");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s7";
	    errorMessage = "Unable to set RFC RetrieveNow parameter with unsigned int value 1";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 7: DESCRIPTION : Clear dcmrfc.log and trigger Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Control.RetrieveNow");
	    LOGGER.info("STEP 7: ACTION : Remove dcmrfc.log and execute webpa or dmcli command to set"
		    + " Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Control.RetrieveNow to unsigned int value 1");
	    LOGGER.info("STEP 7: EXPECTED : Successfully set RFC.RetrieveNow parameter with unsigned int 1");
	    LOGGER.info("**********************************************************************************");

	    BroadBandRfcFeatureControlUtils.clearDCMRFClog(device, tapEnv);
	    tapEnv.executeCommandUsingSsh(device, "rm /tmp/RFC/.hashValue");
	    status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    WebPaParamConstants.WEBPA_PARAM_IMMEDIATE_RFC_CHECK_IN, BroadBandTestConstants.CONSTANT_2,
		    BroadBandTestConstants.STRING_VALUE_ONE);

	    if (status) {
		LOGGER.info("STEP 7: ACTUAL : Successfully set RFC.RetrieveNow parameter with unsigned int 1");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s8";
	    errorMessage = "Unable to find log message with posted parameter value in dcmrfc.log";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 8: DESCRIPTION : Verify RFC processing log messages present in dcmrfc.log"
		    + " for posted parameter value");
	    LOGGER.info("STEP 8: ACTION : Execute command: grep -A 5 -i \"key=tr181.Device.DeviceInfo."
		    + "X_RDKCENTRAL-COM_RFC.Feature.EncryptCloudUpload.Enable value=true\" /rdklogs/logs/dcmrfc.log");
	    LOGGER.info("STEP 8: EXPECTED : Log message with posted tr181 parameter and value is present");
	    LOGGER.info("**********************************************************************************");

	    response = BroadBandRfcFeatureControlUtils.searchFeatureLogsInDcmRfcLog(device, tapEnv,
		    WebPaParamConstants.WEBPA_PARAM_TO_ENABLE_DISABLE_CLOUD_UPLOAD_ENCRYPTION,
		    BroadBandTestConstants.TRUE);
	    status = CommonMethods.isNotNull(response);

	    if (status) {
		LOGGER.info("STEP 8: ACTUAL : Log message with posted tr181 parameter and value is present");
	    } else {
		LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s9";
	    errorMessage = "Unable to find old parameter value true logged in dcmrfc.log for parameter processed by RFC";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 9: DESCRIPTION : Verify old parameter value is logged in dcmrfc.log for the posted parameter");
	    LOGGER.info("STEP 9: ACTION : Verify output of step 8 contains: RFC: old parameter value true");
	    LOGGER.info("STEP 9: EXPECTED : Old parameter value log message is present in output");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.patternMatcher(response, BroadBandCommonUtils.concatStringUsingStringBuffer(
		    BroadBandTraceConstants.LOG_MESSAGE_OLD_PARAM_VALUE, BroadBandTestConstants.TRUE));

	    if (status) {
		LOGGER.info("STEP 9: ACTUAL : Old parameter value log message is present in output");
	    } else {
		LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s10";
	    errorMessage = "Unable to find new parameter value true logged in dcmrfc.log for parameter processed by RFC";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 10: DESCRIPTION : Verify new posted parameter value is logged in dcmrfc.log");
	    LOGGER.info("STEP 10: ACTION : Verify output of step 8 contains: Parameter value  true");
	    LOGGER.info("STEP 10: EXPECTED : New parameter value log message is present in output");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.patternMatcher(response, BroadBandCommonUtils.concatStringUsingStringBuffer(
		    BroadBandTraceConstants.LOG_MESSAGE_NEW_PARAM_VALUE, BroadBandTestConstants.TRUE));

	    if (status) {
		LOGGER.info("STEP 10: ACTUAL : New parameter value log message is present in output");
	    } else {
		LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s11";
	    errorMessage = "Unable to find parameter value same log message in dcmrfc.log for parameter processed by RFC";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 11: DESCRIPTION : Verify parameter old and new values are same log message is present in dcmrfc.log");
	    LOGGER.info("STEP 11: ACTION : Verify output of step 8 contains: RFC: For param Device.DeviceInfo."
		    + "X_RDKCENTRAL-COM_RFC.Feature.EncryptCloudUpload.Enable new and old values are same value true");
	    LOGGER.info(
		    "STEP 11: EXPECTED : RFC parameter old and new values are same log message is present in output");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.patternMatcher(response, BroadBandTraceConstants.LOG_MESSAGE_PARAM_VALUE_SAME);

	    if (status) {
		LOGGER.info(
			"STEP 11: ACTUAL : RFC parameter old and new values are same log message is present in output");
	    } else {
		LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s12";
	    errorMessage = "Value of parameter is not true after being processed by RFC";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 12: DESCRIPTION : Verify parameter value is true after RFC processing");
	    LOGGER.info("STEP 12: ACTION : Execute webpa or dmcli command to get value of "
		    + "Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.EncryptCloudUpload.Enable");
	    LOGGER.info("STEP 12: EXPECTED : Value of  EncryptCloudUpload Enable parameter value is true");
	    LOGGER.info("**********************************************************************************");

	    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    WebPaParamConstants.WEBPA_PARAM_TO_ENABLE_DISABLE_CLOUD_UPLOAD_ENCRYPTION);
	    status = CommonMethods.isNotNull(response) && response.equals(BroadBandTestConstants.TRUE);

	    if (status) {
		LOGGER.info("STEP 12: ACTUAL : Value of  EncryptCloudUpload Enable parameter value is true");
	    } else {
		LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    true);

	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");
	    LOGGER.info(
		    "POST-CONDITION : DESCRIPTION : Remove nvram override of rfc.properties and disable tr181 parameter.");
	    LOGGER.info("POST-CONDITION : ACTION : Remove /nvram/rfc.properties and execute webpa or dmcli command to"
		    + " set value of Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.EncryptCloudUpload.Enable to false");
	    LOGGER.info("POST-CONDITION : EXPECTED : WebPA set is successful and parmeter value is false");

	    status = CommonUtils.removeFileandVerifyFileRemoval(tapEnv, device,
		    BroadBandRfcFeatureControlUtils.NVRAM_RFC_PROPERTIES)
		    && BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			    WebPaParamConstants.WEBPA_PARAM_TO_ENABLE_DISABLE_CLOUD_UPLOAD_ENCRYPTION,
			    BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.FALSE);

	    if (status) {
		LOGGER.info("POST-CONDITION : ACTUAL : Post condition executed successfully");
	    } else {
		LOGGER.error("POST-CONDITION : ACTUAL : Post condition failed");
	    }
	    LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-RFC-1003");
	LOGGER.info("#######################################################################################");
    }

}